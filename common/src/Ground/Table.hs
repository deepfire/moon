{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-unticked-promoted-constructors -Wno-orphans #-}
--{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -dth-dec-file #-}

module Ground.Table
  ( mkValue
  , SomeValueKinded(..)
  , SomeValue(..)
  , readSomeValue
  , parseSomeValue
  , unitSomeValue
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
  , parseExpr
  --
  , VTag'(..)
  )
where

import           Control.Monad.Fail (MonadFail)
import           Codec.Serialise
import           Codec.CBOR.Encoding                (Encoding, encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (Decoder, decodeListLen, decodeWord)
import           Control.Monad                      (forM, unless)
import           Data.GADT.Compare
import qualified Data.Kind                        as K
import qualified Data.Set.Monad                   as Set
import qualified Data.SOP                         as SOP
import           Type.Reflection                    ((:~~:)(..), eqTypeRep, typeRepKind, withTypeable, typeRepKind)

import Text.Read (Lexeme(..), ReadPrec(..), lexP)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Parsers
import Text.Parser.Token.Highlight

import Basis
import qualified Data.Dict as Dict
import Data.Parsing

import Dom.CTag
import Dom.Expr
import Dom.Ground
import Dom.Located
import Dom.Name
import Dom.Parse
import Dom.Some
import Dom.SomeType
import Dom.SomeValue
import Dom.SomeVTag
import Dom.Tags
import Dom.Value
import Dom.VTag

import Ground.Parser ()
import qualified Ground.Hask as Hask

import Namespace
import Pipe.Types
import Wire.Protocol


-- * Tables
--

deriving instance Typeable Pipe
deriving instance Typeable Scope


defineGroundTypes [d|
  data VTag' a where
    -- Meta
    VCon             :: Con
    VExpr            :: Expr (Located (QName Pipe))
    -- VGround          :: Dict Ground
    VPipe            :: SomePipe ()
    VPipeSpace       :: PipeSpace (SomePipe ())
    VQNameScope      :: QName Scope
    VSig             :: ISig
    VSomeType        :: SomeType
    VStruct          :: Struct

    VTypeRep         :: SomeTypeRep
    VTypeRep2        :: (SomeTypeRep, SomeTypeRep)
    VNamePipe        :: Name Pipe
    VQNamePipe       :: QName Pipe

    -- Atom
    VInt             :: Int
    VInteger         :: Integer
    VFloat           :: Float

    VDouble          :: Double
    VString          :: String
    VText            :: Text
    VUnit            :: ()

    -- Common
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

decodeVTop :: Decoder s SomeVTag
decodeVTop = do
  SomeTypeRep (a :: TypeRep b) :: SomeTypeRep <- decode
  case typeRepKind a `eqTypeRep` typeRep @K.Type of
    Just HRefl ->
      pure $ withTypeable a $ SomeVTag $ VTop @b
    Nothing -> error "decodeVTop:  got a non-Type-kinded TypeRep"

deriving instance Eq       (Tags t)

--------------------------------------------------------------------------------
-- * SomeValue literals:  here, due to:
--
--  - VTag variants
--
parseSomeValueLiteral :: Parser SomeValue
parseSomeValueLiteral =
  (SomeValue TPoint . SomeValueKinded VText . mkValue' (Proxy @Text) TPoint <$> stringLiteral)
  <|>
  (SomeValue TPoint . SomeValueKinded VInteger . mkValue' (Proxy @Integer) TPoint <$> integer)
  <|>
  (SomeValue TPoint . SomeValueKinded VInteger . mkValue' (Proxy @Integer) TPoint <$> hexadecimal)
  <|>
  (SomeValue TPoint . SomeValueKinded VDouble . mkValue' (Proxy @Double) TPoint <$> double)

instance Parse SomeValue where
  parser = parseSomeValue parseSomeValueLiteral

unitSomeValue :: SomeValue
unitSomeValue = SomeValue TPoint $ SomeValueKinded VUnit (VPoint ())


-- | Use the ground type table to reconstruct a saturated,
--   and possibly Ground-ed SomePipe.
mkSaturatedPipe
  :: forall c out. (ArgConstr c out)
  => Proxy c -> Tags out -> Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe ()
mkSaturatedPipe _c out name sig struct rep =
  case lookupGroundByRep (someTypeRep $ Proxy @out) of
    Nothing ->
      -- Non-ground (unknown) type, nothing useful we can recapture about it.
      T mempty $
      Pipe (Desc name sig struct rep SOP.Nil out :: Desc Top '[] out)
           ()
    Just (_, _, _, TyDict (_ :: Ground b' => Proxy b')) ->
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
      in encodeListLen ((1 + nArgs) * 4 + 4)
         <> mconcat (encodeTagss $ out SOP.:* args)
         <> encode name
         <> encode sig
         <> encode struct
         <> encode rep
   where
     encodeTagss :: All Top xs => NP Tags xs -> [Encoding]
     encodeTagss = SOP.hcollapse . SOP.hliftA
       (\(Tags (t :: CTag c) (v :: VTag a))
         -> SOP.K $  encode (SomeCTag t)
                  <> encode (SomeVTag v)
                  <> encode (typeRep @c)
                  <> encode (typeRep @a))
  decode = do
    qName <- decode
    len <- decodeListLen
    let arity' = len - 4
        (arity, err) = arity' `divMod` 4
    unless (err == 0 && arity > 0)
      (fail $ "decode SomePipe: expected list len=4+4x && >= 7, got: " <> show len)
    xs :: [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
      <- forM [0..(arity - 1)] $ const $
         (,,,) <$> decode <*> decode <*> decode <*> decode
    name   :: Name Pipe   <- decode
    sig    :: ISig        <- decode
    struct :: Struct      <- decode
    rep    :: SomeTypeRep <- decode
    pure $ somePipeSetQName qName $ withRecoveredTags (head xs) $
      -- Start with a saturated pipe, and then build it up with arguments.
      \out _ _ -> go xs $
        mkSaturatedPipe (Proxy @Top) out name sig struct rep
   where
     go :: [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
        -> SomePipe () -> SomePipe ()
     go []     p = p
     go (x:xs) p =
       withSomePipe p $ \(Pipe (Desc{..} :: Desc c cas o) _) ->
       withRecoveredTags x $
         \(tip :: Tags ca)
          (_ :: Proxy (CTagOf ca)) (_ :: Proxy (TypeOf ca))
         -> go xs $ T mempty $ Pipe
            (Desc pdName pdSig pdStruct pdRep (tip SOP.:* pdArgs) pdOut
             :: Desc Top (ca:cas) o) ()

     withRecoveredTags
       :: forall b
       . (SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)
       -> (forall (c1 :: Con) (a1 :: *) ty
           . ( Typeable (Types c1 a1), Typeable c1, Typeable a1
             , ReifyCTag c1, ReifyVTag a1
             , ty ~ Types c1 a1)
           => Tags ty -> Proxy c1 -> Proxy a1 -> b)
       -> b
     withRecoveredTags
       ( SomeCTag    (tc :: CTag    c),  SomeVTag    (tv :: VTag    a)
       , SomeTypeRep (rc :: TypeRep cr), SomeTypeRep (rv :: TypeRep ar)
       ) f =
       case (rc `eqTypeRep` typeRep @c,  rv `eqTypeRep` typeRep @a)
       of
         (Just HRefl, Just HRefl) ->
           withTypeable rc $ withTypeable rv $ withReifyCTag tc $
             f (Tags tc tv :: Tags (Types c a))
               (Proxy @c) (Proxy @a)
         (,) Nothing _ -> error $ mconcat
           [ "withRecoveredTags: container tag miss: Ctag c=", show $ typeRep @c
           , " rc=", show rc
           ]
         (,) _ Nothing -> error $ mconcat
           [ "withRecoveredTags: value tag miss: VTag a=", show $ typeRep @a
           , " rv=", show rv
           ]
