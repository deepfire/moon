{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -dth-dec-file #-}

module Ground.Table (module Ground.Table) where

import Data.GADT.Compare
import Data.SOP                         qualified as SOP
import Data.Dynamic                     qualified as Dyn

import Text.Megaparsec.Parsers

import Basis
import Data.Parsing
import Data.TyDict                      qualified as TyDict

import Dom.CTag
import Dom.Cap
import Dom.Expr
import Dom.Ground
import Dom.Ground.Hask                  qualified as Hask
import Dom.Located
import Dom.LTag
import Dom.Name
import Dom.Parse
import Dom.Pipe
import Dom.Scope
import Dom.Sig
import Dom.SomePipe
import Dom.SomeType
import Dom.SomeValue
import Dom.SomeVTag
import Dom.Space.Pipe
import Dom.Struct
import Dom.Tags
import Dom.Value
import Dom.VTag

import Distribution.Utils.ShortText           (ShortText)
import Distribution.Types.PackageName         (PackageName)

instance Serialise Distribution.Utils.ShortText.ShortText
instance Serialise PackageName


-- * Tables
--
defineGroundTypes [d|
  data VTag' a where
    -- Meta
    VCon             :: Con
    VExpr            :: Expr (Located (QName Pipe))
    -- VGround          :: Dict Ground
    VPipe            :: SomePipe ()
    VPipeSpace       :: TSG PipeSpace (SomePipe ())
    VPipeSpaceDyn    :: PipeSpace (SomePipe Dyn.Dynamic)
    VQNameScope      :: QName Scope
    VSig             :: ISig
    VSomeType        :: SomeType
    VStruct          :: Struct

    VTypeRep         :: SomeTypeRep
    VTypeRep2        :: (SomeTypeRep, SomeTypeRep)
    VNamePipe        :: Name Pipe
    VQNamePipe       :: QName Pipe

    -- Atom
    VUnit            :: ()
    VBool            :: Bool

    VInt             :: Int
    VInteger         :: Integer
    VFloat           :: Float
    VDouble          :: Double

    VString          :: String
    VText            :: Text

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

    -- VCabalPackageName :: PackageName

    -- Top, special processing.
    VTop             :: a
 |]

sealGround :: IO ()
sealGround = setupGroundTypes groundTable

decodeVTop :: Decoder s SomeVTag
decodeVTop = do
  SomeTypeRep (a :: TypeRep b) :: SomeTypeRep <- decode
  case typeRepKind a `eqTypeRep` typeRep @Type of
    Just HRefl ->
      pure $ withTypeable a $ SomeVTag $ VTop @b
    Nothing -> error "decodeVTop:  got a non-Type-kinded TypeRep"

deriving instance Eq       (Tags t)

-- xs :: [SomePipe Dynamic]
-- xs = dataProjScope'
--        (Proxy @Foo)
--        $(dataProjPipeScope (Proxy @Foo))

--------------------------------------------------------------------------------
-- * Depends on Serialise SomeVTag, which comes from the ground table.
--
instance Serialise (SomePipe ()) where
  encode p =
    withSomePipe p $
     \(Pipe (Desc name sig struct rep ltag args out :: Desc l args out) _) ->
      let nArgs = fromIntegral . SOP.lengthSList $ Proxy @args
      in encodeListLen (5 + (1 + nArgs) * 4)
         <> encode (somePipeQName p)
         <> encode name
         <> encode sig
         <> encode struct
         <> encode rep
         <> encode (SomeLTag ltag)
         -- NOTE: out first, then args:
         <> mconcat (encodeTagss $ out :* args)
   where
     encodeTagss :: All Top xs => NP Tags xs -> [Encoding]
     encodeTagss = SOP.hcollapse . SOP.hliftA
       (\(Tags (c :: CTag c) (v :: VTag a))
         -> SOP.K $ withVTag v (   encode (SomeCTag c)
                                <> encode (SomeVTag v)
                                <> encode (typeRep @c)
                                <> encode (typeRep @a)))
  decode :: Decoder s (SomePipe ())
  decode = do
    len <- decodeListLen
    let (arityPlusOne, err) = (len - 5) `divMod` 4
    unless (err == 0 && arityPlusOne > 0)
      (fail $ "decode SomePipe: expected list len=5+4x && x >= 1, got: " <> show len)
    either fail pure =<< recoverPipe
      <$> (decode :: Decoder s (QName Pipe))
      <*> (decode :: Decoder s (Name Pipe))
      <*> (decode :: Decoder s ISig)
      <*> (decode :: Decoder s Struct)
      <*> (decode :: Decoder s SomeTypeRep)
      <*> (decode :: Decoder s SomeLTag)
      -- NOTE: out first, then args:
      <*> (forM [0..(arityPlusOne - 1)] $ const $
            (,,,) <$> decode <*> decode <*> decode <*> decode
           :: Decoder s [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)])

--------------------------------------------------------------------------------
-- * SomeValue literals:  here, due to:
--
--  - VTag variants
--
parseSomeValueLiteral :: Parser SomeValue
parseSomeValueLiteral =
  (SV CPoint . SVK VText    capsTSG . mkValue' (Proxy @Text)    CPoint <$> stringLiteral)
  <|>
  (SV CPoint . SVK VInteger capsTSG . mkValue' (Proxy @Integer) CPoint <$> integer)
  <|>
  (SV CPoint . SVK VInteger capsTSG . mkValue' (Proxy @Integer) CPoint <$> hexadecimal)
  <|>
  (SV CPoint . SVK VDouble  capsTSG . mkValue' (Proxy @Double)  CPoint <$> double)

instance Parse SomeValue where
  parser = parseSomeValue parseSomeValueLiteral

someValueUnit :: SomeValue
someValueUnit = mkSomeValue CPoint VUnit capsTSG ()

someValueText :: Text -> SomeValue
someValueText = mkSomeValue CPoint VText capsTSG
