{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-unticked-promoted-constructors -Wno-orphans #-}
--{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -dth-dec-file #-}

module Ground.Table
  ( mkValue
  , SomeValueKinded(..)
  , SomeValue(..)
  , mkSomeValue
  --
  , lookupRep
  , lookupName
  , lookupNameRep
  , withRepGroundType
  , withNameGroundType
  , groundTypeReps
  , groundTypeNames
  --
  , parseDict
  --
  , VTag(..)
  , ReifyVTag(..)
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
import SomeType

import Data.Parsing
import Ground.Parser ()
import Ground.TH
import qualified Ground.Hask as Hask
import Namespace
import Pipe.Types


-- * Tables
--
deriving instance Typeable Pipe
deriving instance Typeable Scope
deriving instance Typeable Type

defineGroundTypes [d|
  data VTag a where
    -- Meta
    -- VGround       :: Dict Ground
    VCon             :: Con
    VSomeType        :: SomeType
    VSig             :: ISig
    VStruct          :: Struct
    VPipe            :: SomePipe ()
    VTypeRep         :: SomeTypeRep
    VTypeRep2        :: (SomeTypeRep, SomeTypeRep)
    VNameType        :: Name Type
    VNamePipe        :: Name Pipe
    VQNamePipe       :: QName Pipe
    VQNameScope      :: QName Scope
    -- OMG, Ord on PipeSpace..
    VPipeSpace       :: PipeSpace (SomePipe ())
    -- Atom
    VInt             :: Int
    VInteger         :: Integer
    VFloat           :: Float
    VDouble          :: Double
    VString          :: String
    VText            :: Text
    VUnit            :: ()
    -- Plain
    VFileName        :: Hask.FileName
    VLoc             :: Hask.Loc
    VURL             :: Hask.URL
    -- Hask
    VNameHaskIndex   :: Name Hask.Index
    VHaskIndex       :: Hask.Index
    VNameHaskRepo    :: Name Hask.Repo
    VNameHaskPackage :: Name Hask.Package
    VNameHaskModule  :: Name Hask.Module
    VNameHaskDef     :: Name Hask.Def
    VHaskDef         :: Hask.Def
    VHaskDefType     :: Hask.DefType
    -- Top, special processing.
    VTop             :: a
 |]

class ReifyVTag (a :: *) where
  reifyVTag :: Proxy a -> VTag a

--------------------------------------------------------------------------------
-- * SomeValue
--
data SomeValueKinded (c :: Con)
  = forall a. Ground a => SomeValueKinded (VTag a) (Value c a)

data SomeKindValue a =
  forall (c :: Con). Typeable c =>
  SomeKindValue (CTag c) (Value c a)

data SomeValue =
  forall c. (ReifyCTag c, Typeable c) =>
  SomeValue (CTag c) (SomeValueKinded c)

-- data SomeValue =
--   forall a. Ground a =>
--   SomeValue  (SomeKindValue a)

--------------------------------------------------------------------------------
-- * Value
--
mkValue :: VTag a -> CTag k -> Repr k a -> Value k a
mkValue = const $ \case
  TPoint -> VPoint
  TList  -> VList
  TSet   -> VSet
  TTree  -> VTree
  TDag   -> VDag
  TGraph -> VGraph


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
  => Proxy c -> TypePair out -> Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe ()
mkSaturatedPipe _c out name sig struct rep =
  case lookupRep (someTypeRep $ Proxy @out) of
    Nothing ->
      -- Non-ground (unknown) type, nothing useful we can recapture about it.
      T mempty $
      Pipe (Desc name sig struct rep SOP.Nil out :: Desc Top '[] out)
           ()
    Just (TyDict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @(TypeOf out) of
        Just HRefl ->
          G mempty
          (Pipe (Desc name sig struct rep SOP.Nil out :: Desc Ground '[] out)
                () :: Pipe Ground '[] out ())
        Nothing -> error $
          "mkSaturatedPipe:  internal inconsistency while looking up Dict for type "
          <> unpack (showName name)

instance Serialise (SomePipe ()) where
  encode p =
    (encode (somePipeQName p) <>) $
    withSomePipe p $
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
       (\(TypePair (t :: CTag c) (a :: Proxy a))
         -> SOP.K $  encode (SomeCTag t)
                  <> encode (typeRep @c)
                  <> encode (someTypeRep a))
  decode = do
    qName <- decode
    len <- decodeListLen
    let arity' = len - 4
        (arity, err) = arity' `divMod` 3
    unless (err == 0 && arity > 0)
      (fail $ "decode SomePipe: expected list len=4+3x && >= 7, got: " <> show len)
    xs :: [(SomeCTag, SomeTypeRep, SomeTypeRep)]
      <- forM [0..(arity - 1)] $ const $
         (,,) <$> decode <*> decode <*> decode
    name   :: Name Pipe   <- decode
    sig    :: ISig        <- decode
    struct :: Struct      <- decode
    rep    :: SomeTypeRep <- decode
    pure $ somePipeSetQName qName $ withRecoveredTypePair (head xs) $
      -- Start with a saturated pipe, and then build it up with arguments.
      \out _ _ -> go xs $
        mkSaturatedPipe (Proxy @Top) out name sig struct rep
   where
     go :: [(SomeCTag, SomeTypeRep, SomeTypeRep)]
        -> SomePipe () -> SomePipe ()
     go []     p = p
     go (x:xs) p =
       withSomePipe p $ \(Pipe (Desc{..} :: Desc c cas o) _) ->
       withRecoveredTypePair x $
         \(tip :: TypePair ca)
          (_ :: Proxy (CTagOf ca)) (_ :: Proxy (TypeOf ca))
         -> go xs $ T mempty $ Pipe
            (Desc pdName pdSig pdStruct pdRep (tip SOP.:* pdArgs) pdOut
             :: Desc Top (ca:cas) o) ()

     withRecoveredTypePair
       :: forall b
       . (SomeCTag, SomeTypeRep, SomeTypeRep)
       -> (forall (c1 :: Con) (a1 :: *) ty
           . ( Typeable (Type c1 a1), Typeable c1, Typeable a1
             , ReifyCTag c1
             , ty ~ Type c1 a1)
           => TypePair ty -> Proxy c1 -> Proxy a1 -> b)
       -> b
     withRecoveredTypePair
       ( SomeCTag    (ta :: CTag     c)
       , SomeTypeRep (ca :: TypeRep ca)
       , SomeTypeRep  (a :: TypeRep  a)
       ) f =
       case (,,)
            (typeRepKind ca `eqTypeRep` typeRep @Con)
            (ca             `eqTypeRep` typeRep @c)
            (typeRepKind a  `eqTypeRep` typeRep @K.Type)
       of
         (Just HRefl, Just HRefl, Just HRefl) ->
           withTypeable ca $ withTypeable  a $ withReifyCTag ta $
             f (TypePair (reifyCTag (Proxy @ca)) (Proxy @a)
                 :: TypePair (Type c a))
               (Proxy @ca) (Proxy @a)
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
