{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -dth-dec-file #-}

module Ground.Table (module Ground.Table) where

import           Control.Monad.Fail (MonadFail)
import           Codec.Serialise
import           Codec.CBOR.Encoding                (Encoding, encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (Decoder, decodeListLen, decodeWord)
import           Control.Monad                      (forM, unless)
import           Data.GADT.Compare
import qualified Data.Kind                        as K
import qualified Data.Set.Monad                   as Set
import qualified Data.SOP                         as SOP

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
import qualified Dom.Ground.Hask as Hask
import Dom.Located
import Dom.Name
import Dom.Parse
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Sig
import Dom.Some
import Dom.SomeType
import Dom.SomeValue
import Dom.SomeVTag
import Dom.Space.Pipe
import Dom.Struct
import Dom.Tags
import Dom.Value
import Dom.VTag



-- * Tables
--
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

sealGround :: IO ()
sealGround = setupGroundTypes groundTable

decodeVTop :: Decoder s SomeVTag
decodeVTop = do
  SomeTypeRep (a :: TypeRep b) :: SomeTypeRep <- decode
  case typeRepKind a `eqTypeRep` typeRep @K.Type of
    Just HRefl ->
      pure $ withTypeable a $ SomeVTag $ VTop @b
    Nothing -> error "decodeVTop:  got a non-Type-kinded TypeRep"

deriving instance Eq       (Tags t)

--------------------------------------------------------------------------------
-- * Depends on Serialise SomeVTag, which comes from the ground table.
--
instance Serialise (SomePipe ()) where
  encode p =
    withSomePipe p $
     \(Pipe (Desc name sig struct rep args out :: Desc c args out) _) ->
      let nArgs = fromIntegral . SOP.lengthSList $ Proxy @args
      in encodeListLen (5 + (1 + nArgs) * 4)
         <> encode (somePipeQName p)
         <> encode name
         <> encode sig
         <> encode struct
         <> encode rep
         <> mconcat (encodeTagss $ out :* args)
   where
     encodeTagss :: All Top xs => NP Tags xs -> [Encoding]
     encodeTagss = SOP.hcollapse . SOP.hliftA
       (\(Tags (t :: CTag c) (v :: VTag a))
         -> SOP.K $  encode (SomeCTag t)
                  <> encode (SomeVTag v)
                  <> encode (typeRep @c)
                  <> encode (typeRep @a))
  decode :: Decoder s (SomePipe ())
  decode = do
    len <- decodeListLen
    let (arity, err) = (len - 5) `divMod` 4
    unless (err == 0 && arity > 0)
      (fail $ "decode SomePipe: expected list len=5+4x && >= 9, got: " <> show len)
    recoverPipe
      <$> (decode :: Decoder s (QName Pipe))
      <*> (decode :: Decoder s (Name Pipe))
      <*> (decode :: Decoder s ISig)
      <*> (decode :: Decoder s Struct)
      <*> (decode :: Decoder s SomeTypeRep)
      <*> (forM [0..(arity - 1)] $ const $
            (,,,) <$> decode <*> decode <*> decode <*> decode
           :: Decoder s [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)])

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

someValueUnit :: SomeValue
someValueUnit = SomeValue TPoint $ SomeValueKinded VUnit (VPoint ())

someValueText :: Text -> SomeValue
someValueText = mkSomeGroundValue TPoint VText
