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

import           Codec.Serialise
import           Codec.CBOR.Encoding                (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (decodeListLen, decodeWord)
import           Control.Monad                      (unless)
import qualified Data.Kind                        as K
import           Data.Reflection
import           Type.Reflection                    ((:~~:)(..), TypeRep, eqTypeRep, typeRepKind, withTypeable)

import qualified Data.Set.Monad                   as Set
import Text.Read (Lexeme(..), ReadPrec, lexP)
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token.Highlight
import Text.Parser.Token

import Basis
import qualified Data.Dict as Dict
import Data.Dict (Dict(..), Dicts)
import Type

import Ground.Parser ()
import qualified Ground.Hask as Hask
import Namespace
import Pipe.Types


-- * Ground API
--
lookupRep :: SomeTypeRep -> Maybe (Dict Ground)
lookupRep = Dict.lookupRep groundTypes

lookupName :: Text       -> Maybe (Dict Ground)
lookupName = Dict.lookupName groundTypes

lookupNameRep :: Name Type -> Maybe SomeTypeRep
lookupNameRep (Name n) = Dict.lookupNameRep groundTypes n

withRepGroundType :: SomeTypeRep -> (Dict Ground -> b) -> Maybe b
withRepGroundType  str f = f <$> lookupRep str

withNameGroundType :: Text -> (Dict Ground -> b) -> Maybe b
withNameGroundType str f = f <$> lookupName str

groundTypeReps :: [SomeTypeRep]
groundTypeReps = Dict.reps groundTypes

groundTypeNames :: [Text]
groundTypeNames = Dict.names groundTypes


-- * Dict Ground
--
instance Parser (Dict Ground) where
  parser = parseDict

parseDict :: (Monad m, TokenParsing m) => m (Dict Ground)
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

instance Read (Dict Ground) where
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
instance Parser SomeValue where
  parser = parseSomeValue

parseSomeValue :: (Monad m, TokenParsing m) => m SomeValue
parseSomeValue =
  braces   (parseSV . reifyTag $ Proxy @Point)
  <|>
  brackets (parseSV . reifyTag $ Proxy @List)
  <?> "Value"
 where
  parseSV :: (Monad m, TokenParsing m) => Tag k -> m SomeValue
  parseSV tag = do
    Dict a :: Dict Ground <- parser
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

readValue :: forall (k :: Con). Tag k -> Dict Ground -> ReadPrec SomeValue
readValue tag (Dict (a :: Proxy a)) = do
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
    dict :: Dict Ground <- readPrec
    case tag of
      Exists tag' -> readValue tag' dict


mkSomeDesc
  :: forall (ka :: Con) (a :: *) (kb :: Con) (b :: *)
  . ( ReifyTag ka, ReifyTag kb
    , Typeable ka, Typeable a, Typeable kb, Typeable b
    )
  => Tag ka -> Proxy a -> Tag kb -> Proxy b
  -> Name Pipe -> Sig -> Struct -> SomeTypeRep -> SomeDesc
mkSomeDesc ta a tb b name sig struct rep =
  case lookupRep (someTypeRep b) of
    Nothing ->
      SomeDesc $
      (Desc name sig struct rep ta a tb b
        :: Desc Top ka a kb b)
    Just (Dict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @b of
        Just HRefl ->
          SomeDesc $ (Desc name sig struct rep ta a tb b :: Desc Ground ka a kb b)
        Nothing ->
          SomeDesc $ (Desc name sig struct rep ta a tb b :: Desc Top    ka a kb b)

instance Serialise SomeDesc where
  encode (SomeDesc (Desc name sig struct rep _ _ _ _ :: Desc c ka a kb b)) = do
    encodeListLen 10
    <> encode (SomeTag . reifyTag $ Proxy @ka)
    <> encode (typeRep @ka)
    <> encode (tRep $ sIn  sig)
    <> encode (SomeTag . reifyTag $ Proxy @kb)
    <> encode (typeRep @kb)
    <> encode (tRep $ sOut sig)
    <> encode name
    <> encode sig
    <> encode struct
    <> encode rep
  decode = do
    len <- decodeListLen
    unless (len == 10)
      (fail $ "decode SomeDesc: expected list len=8, got: " <> show len)
    sta    :: SomeTag     <- decode
    strka  :: SomeTypeRep <- decode
    stra   :: SomeTypeRep <- decode
    stb    :: SomeTag     <- decode
    strkb  :: SomeTypeRep <- decode
    strb   :: SomeTypeRep <- decode
    name   :: Name Pipe   <- decode
    sig    :: Sig         <- decode
    struct :: Struct      <- decode
    rep    :: SomeTypeRep <- decode
    case (,,,,,) sta strka stra stb strkb strb of
      (,,,,,)
        (SomeTag (ta :: Tag tka)) (SomeTypeRep (ka :: TypeRep ka)) (SomeTypeRep (a :: TypeRep a))
        (SomeTag (tb :: Tag tkb)) (SomeTypeRep (kb :: TypeRep kb)) (SomeTypeRep (b :: TypeRep b))
        -> case (,,,,,)
                (typeRep @K.Type `eqTypeRep` typeRepKind a)
                (typeRep @K.Type `eqTypeRep` typeRepKind b)
                (typeRep @Con    `eqTypeRep` typeRepKind ka)
                (typeRep @Con    `eqTypeRep` typeRepKind kb)
                (typeRep @tka    `eqTypeRep` ka)
                (typeRep @tkb    `eqTypeRep` kb)
                of
             (Just HRefl, Just HRefl, Just HRefl, Just HRefl, Just HRefl, Just HRefl) ->
               withTypeable ka $ withTypeable kb $
               withTypeable  a $ withTypeable  b $
               withReifyTag ta $ withReifyTag tb $
                 pure $ mkSomeDesc ta (Proxy @a) tb (Proxy @b) name sig struct rep
             _ -> fail $ "decode SomeDesc: something went awry."

instance Serialise (SomePipe ()) where
  encode (G (Pipe desc ())) = encode (SomeDesc desc)
  encode (T (Pipe desc ())) = encode (SomeDesc desc)
  decode = do
    sd <- decode
    case sd of
      (SomeDesc (d@Desc{} :: Desc c ka a kb b)) ->
        case ( eqTypeRep (typeRep @c) (typeRep @(Ground :: * -> Constraint))
             , eqTypeRep (typeRep @c) (typeRep @(Top    :: * -> Constraint))
             ) of
          (Just HRefl, Nothing)    -> pure . G $ Pipe d ()
          (Nothing,    Just HRefl) -> pure . T $ Pipe d ()
          (eg,         et) -> fail $ "SomeDesc failed decode: "
            <>" eqTypeRep @c @Ground="<>show eg
            <>" eqTypeRep @c @Top="<>show et
            <>" rep:\n"<>unpack (showDesc d)


-- * Tables
--
infixr 2 #
(#) :: (a -> b) -> a -> b
(#) = ($)

deriving instance Typeable Pipe
deriving instance Typeable Scope

groundTypes :: Dicts Ground
groundTypes = Dict.empty
  -- Meta
  -- & Dict.insert "Ground"          # Proxy @(Dict Ground)
  & Dict.insert "Con"             # Proxy @Con
  & Dict.insert "Type"            # Proxy @Type
  & Dict.insert "Sig"             # Proxy @Sig
  & Dict.insert "Struct"          # Proxy @Struct
  & Dict.insert "PipeDesc"        # Proxy @SomeDesc
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
