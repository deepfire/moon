{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-unticked-promoted-constructors -Wno-orphans #-}

module Ground.Table
  ( lookupRep
  , lookupName
  , lookupNameRep
  , withRepGroundType
  , withNameGroundType
  , groundTypeReps
  , groundTypeNames
  --
  , parseDict
  , parseSomeValue
  )
where

import           Control.Monad.Fail (MonadFail)
import           Codec.Serialise
import           Codec.CBOR.Encoding                (Encoding, encodeListLen)
import           Codec.CBOR.Decoding                (decodeListLen)
import           Control.Monad                      (forM, unless)
import qualified Data.Kind                        as K
import qualified Data.SOP                         as SOP
import           Type.Reflection                    ((:~~:)(..), eqTypeRep, typeRepKind, withTypeable)

import qualified Data.Set.Monad                   as Set
import Text.Read (Lexeme(..), ReadPrec, lexP)
import Text.Megaparsec hiding (ParsecT)
import Text.Megaparsec.Parsers
import Text.Parser.Token.Highlight

import Basis
import qualified Data.Dict as Dict
import Type

import Data.Parsing
import Ground.Parser ()
import qualified Ground.Hask as Hask
import Namespace
import Pipe.Types


-- * Ground API
--
lookupRep :: SomeTypeRep -> Maybe (TyDict Ground)
lookupRep = Dict.lookupRep groundTypes

lookupName :: Text       -> Maybe (TyDict Ground)
lookupName = Dict.lookupName groundTypes

lookupNameRep :: Name Type -> Maybe SomeTypeRep
lookupNameRep (Name n) = Dict.lookupNameRep groundTypes n

withRepGroundType :: SomeTypeRep -> (TyDict Ground -> b) -> Maybe b
withRepGroundType  str f = f <$> lookupRep str


withNameGroundType :: Text -> (TyDict Ground -> b) -> Maybe b
withNameGroundType str f = f <$> lookupName str

groundTypeReps :: [SomeTypeRep]
groundTypeReps = Dict.reps groundTypes

groundTypeNames :: [Text]
groundTypeNames = Dict.names groundTypes


-- * Dict Ground
--
instance Parse (TyDict Ground) where
  parser = parseDict

parseDict :: (MonadFail m, TokenParsing m) => m (TyDict Ground)
parseDict = do
  i <- identifier
  case Ground.Table.lookupName i of
    Just x -> pure x
    Nothing -> trace (printf "Unknown ground: %s" i)
                     (fail $ "Unknown ground: " <> show i)
 where
   identifier :: (Monad m, TokenParsing m) => m Text
   identifier = ident $ IdentifierStyle
     { _styleName = "Ground tag"
     , _styleStart = letter
     , _styleLetter = alphaNum
     , _styleReserved = mempty
     , _styleHighlight = Identifier
     , _styleReservedHighlight = ReservedIdentifier
     }

instance Read (TyDict Ground) where
  readPrec = do
    ty <- lexP
    case ty of
      Ident i -> case Ground.Table.lookupName (pack i) of
        Just x -> pure x
        Nothing -> trace (printf "Unknown ground: %s" i)
                         (fail $ "Unknown ground: " <> i)
      x -> fail $ "Unexpected construct: " <> show x


-- * SomeValue
--
instance Parse SomeValue where
  parser = parseSomeValue

parseSomeValue :: Parser SomeValue
parseSomeValue =
  braces   (parseSV . reifyTag $ Proxy @Point)
  <|>
  brackets (parseSV . reifyTag $ Proxy @List)
 where
  parseSV :: Tag k -> Parser SomeValue
  parseSV tag = do
    TyDict a :: TyDict Ground <- parser
    case tag of
      TPoint -> do
        v :: a <- parser
        pure $ SomeValue $ SomeKindValue tag $ mkValue' a tag v
      TList -> do
        v :: [a] <- commaSep parser
        pure $ SomeValue $ SomeKindValue tag $ mkValue' a tag v
      TSet -> do
        v :: [a] <- commaSep parser
        pure $ SomeValue $ SomeKindValue tag $ mkValue' a tag $ Set.fromList v
      _ -> trace (printf "No parser for structures outside of Point/List/Set.")
                 (fail "")

readValue :: forall (k :: Con). Tag k -> TyDict Ground -> ReadPrec SomeValue
readValue tag (TyDict (a :: Proxy a)) =
  case tag of
    TPoint -> do
      v :: a <- readPrec
      pure $ SomeValue $ SomeKindValue tag $ mkValue' a tag v
    TList -> do
      v :: [a] <- readListPrec
      pure $ SomeValue $ SomeKindValue tag $ mkValue' a tag v
    TSet -> do
      v :: [a] <- readListPrec
      pure $ SomeValue $ SomeKindValue tag $ mkValue' a tag $ Set.fromList v
    _ -> trace (printf "No parser for structures outside of Point/List/Set.")
               (fail "")

instance Read SomeValue where
  readPrec = do
    tag :: Some Tag <- readPrec
    dict :: TyDict Ground <- readPrec
    case tag of
      Exists tag' -> readValue tag' dict


_withGroundTop
  :: forall out b
  .  Typeable out
  => out
  -> (forall a. Ground a => a -> b)
  -> (forall a. Top    a => a -> b)
  -> b
_withGroundTop out ground top =
  case lookupRep (someTypeRep $ Proxy @out) of
    Nothing -> top out
    Just (TyDict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @out of
        Nothing -> top out
        Just HRefl -> ground out

-- | Use the ground type table to reconstruct a saturated,
--   and possibly Ground-ed SomePipe.
mkSaturatedPipe
  :: forall c out. (ArgConstr c out)
  => Proxy c -> TypePair out -> Name Pipe -> Sig -> Struct -> SomeTypeRep -> SomePipe ()
mkSaturatedPipe _c out name sig struct rep =
  case lookupRep (someTypeRep $ Proxy @out) of
    Nothing -> nondescript
    Just (TyDict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @(TypeOf out) of
        Nothing -> nondescript
        Just HRefl ->
          G (Pipe (Desc name sig struct rep SOP.Nil out :: Desc Ground '[] out) () :: Pipe Ground '[] out ())
 where
   -- Non-ground (unknown) type, nothing useful we can recapture about it.
   nondescript =
     T $ Pipe (Desc name sig struct rep SOP.Nil out :: Desc Top '[] out) ()

instance Serialise (SomePipe ()) where
  encode p = withSomePipe p $
    \(Pipe (Desc name sig struct rep args out :: Desc c args out) _) ->
      let nArgs = fromIntegral . SOP.lengthSList $ Proxy @args
      in encodeListLen ((1 + nArgs) * 3 + 4)
         <> mconcat (encodeTypePairs $ out SOP.:* args)
         <> encode name
         <> encode sig
         <> encode struct
         <> encode rep
   where
     encodeTypePairs :: All Top xs => NP TypePair xs -> [Encoding]
     encodeTypePairs = SOP.hcollapse . SOP.hliftA
       (\(TypePair (t :: Tag k) (a :: Proxy a))
         -> SOP.K $  encode (SomeTag t)
                  <> encode (typeRep @k)
                  <> encode (someTypeRep a))
  decode = do
    len <- decodeListLen
    let arity' = len - 4
        (arity, err) = arity' `divMod` 3
    unless (err == 0 && arity > 0)
      (fail $ "decode SomePipe: expected list len=4+3x && >= 7, got: " <> show len)
    xs :: [(SomeTag, SomeTypeRep, SomeTypeRep)]
      <- forM [0..(arity - 1)] $ const $
         (,,) <$> decode <*> decode <*> decode
    name   :: Name Pipe   <- decode
    sig    :: Sig         <- decode
    struct :: Struct      <- decode
    rep    :: SomeTypeRep <- decode
    pure $ withRecoveredTypePair (head xs) $
      -- Start with a saturated pipe, and then build it up with arguments.
      \out _ _ -> go xs $
        mkSaturatedPipe (Proxy @Top) out name sig struct rep
   where
     go :: [(SomeTag, SomeTypeRep, SomeTypeRep)]
        -> SomePipe () -> SomePipe ()
     go []     p = p
     go (x:xs) p =
       withSomePipe p $ \(Pipe (Desc{..} :: Desc c kas o) _) ->
       withRecoveredTypePair x $
         \(tip :: TypePair ka)
          (_ :: Proxy (TagOf ka)) (_ :: Proxy (TypeOf ka))
         -> go xs $ T $ Pipe
            (Desc pdName pdSig pdStruct pdRep (tip SOP.:* pdArgs) pdOut
             :: Desc Top (ka:kas) o) ()

     withRecoveredTypePair
       :: forall b
       . (SomeTag, SomeTypeRep, SomeTypeRep)
       -> (forall (k1 :: Con) (a1 :: *) ty
           . ( Typeable (Type k1 a1), Typeable k1, Typeable a1
             , ReifyTag k1
             , ty ~ Type k1 a1)
           => TypePair ty -> Proxy k1 -> Proxy a1 -> b)
       -> b
     withRecoveredTypePair
       ( SomeTag     (ta :: Tag     k)
       , SomeTypeRep (ka :: TypeRep ka)
       , SomeTypeRep  (a :: TypeRep  a)
       ) f =
       case (,,)
            (typeRepKind ka `eqTypeRep` typeRep @Con)
            (ka             `eqTypeRep` typeRep @k)
            (typeRepKind a  `eqTypeRep` typeRep @K.Type)
       of
         (Just HRefl, Just HRefl, Just HRefl) ->
           withTypeable ka $ withTypeable  a $ withReifyTag ta $
             f (TypePair (reifyTag (Proxy @ka)) (Proxy @a)
                 :: TypePair (Type k a))
               (Proxy @ka) (Proxy @a)
         _ -> error "withRecoveredTypePair"

-- instance Serialise (SomePipe ()) where
--   decode = do
--     sd <- decode
--     case sd of
--       (SomeDesc (d@Desc{} :: Desc c as o)) ->
--         case ( eqTypeRep (typeRep @c) (typeRep @(Ground :: * -> Constraint))
--              , eqTypeRep (typeRep @c) (typeRep @(Top    :: * -> Constraint))
--              ) of
--           (Just HRefl, Nothing)    -> pure . G $ Pipe d ()
--           (Nothing,    Just HRefl) -> pure . T $ Pipe d ()
--           (eg,         et) -> fail $ "SomeDes failed decode: "
--             <>" eqTypeRep @c @Ground="<>show eg
--             <>" eqTypeRep @c @Top="<>show et
--             <>" rep:\n"<>unpack (showDesc d)


-- * Tables
--
infixr 2 #
(#) :: (a -> b) -> a -> b
(#) = ($)

deriving instance Typeable Pipe
deriving instance Typeable Scope
deriving instance Typeable Type

groundTypes :: TyDicts Ground
groundTypes = Dict.empty
  -- Meta
  -- & Dict.insert "Ground"          # Proxy @(Dict Ground)
  & Dict.insert "Con"             # Proxy @Con
  & Dict.insert "SomeType"        # Proxy @SomeType
  & Dict.insert "Sig"             # Proxy @Sig
  & Dict.insert "Struct"          # Proxy @Struct
  & Dict.insert "Pipe"            # Proxy @(SomePipe ())
  & Dict.insert "TypeRep"         # Proxy @SomeTypeRep
  & Dict.insert "TypeRep2"        # Proxy @(SomeTypeRep, SomeTypeRep)
  & Dict.insert "NameType"        # Proxy @(Name Type)
  & Dict.insert "NamePipe"        # Proxy @(Name Pipe)
  & Dict.insert "QNamePipe"       # Proxy @(QName Pipe)
  & Dict.insert "QNameScope"      # Proxy @(QName Scope)
  & Dict.insert "PipeSpace"       # Proxy @(PipeSpace (SomePipe ()))
  -- Atom
  & Dict.insert "Int"             # Proxy @Int
  & Dict.insert "Integer"         # Proxy @Integer
  & Dict.insert "Float"           # Proxy @Float
  & Dict.insert "Double"          # Proxy @Double
  & Dict.insert "String"          # Proxy @String
  & Dict.insert "Text"            # Proxy @Text
  & Dict.insert "Unit"            # Proxy @()
  -- Plain
  & Dict.insert "FileName"        # Proxy @Hask.FileName
  & Dict.insert "Loc"             # Proxy @Hask.Loc
  & Dict.insert "URL"             # Proxy @Hask.URL
  -- Hask
  & Dict.insert "NameHaskIndex"   # Proxy @(Name Hask.Index)
  & Dict.insert "HaskIndex"       # Proxy @Hask.Index
  & Dict.insert "NameHaskRepo"    # Proxy @(Name Hask.Repo)
  & Dict.insert "NameHaskPackage" # Proxy @(Name Hask.Package)
  & Dict.insert "NameHaskModule"  # Proxy @(Name Hask.Module)
  & Dict.insert "NameHaskDef"     # Proxy @(Name Hask.Def)
  & Dict.insert "HaskDef"         # Proxy @Hask.Def
  & Dict.insert "HaskDefType"     # Proxy @Hask.DefType
