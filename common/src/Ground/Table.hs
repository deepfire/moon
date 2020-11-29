{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -dth-dec-file #-}

module Ground.Table (module Ground.Table) where

import qualified Algebra.Graph                    as G
-- import           Codec.Serialise
import           Data.Dynamic                       (toDyn)
import           Data.GADT.Compare
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
import Dom.Pipe.IOA
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
  case typeRepKind a `eqTypeRep` typeRep @Type of
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
         -> SOP.K $ withVTag v (   encode (SomeCTag t)
                                <> encode (SomeVTag v)
                                <> encode (typeRep @c)
                                <> encode (typeRep @a)))
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
-- * Pipe construction:  due to withVTag
--
-- [Note: Type argument numbering]
--
-- In order to maintain stable & simple enumeration of arrow type constituents,
-- the enumeration starts from end -- i.e:
--   - the arrow result is #0
--   - the last function argument, if any, is #1
--   and so on.
pipe0 ::
  forall c ioa c0 t0.
  ( Typeable c
  , ioa ~ IOA c '[] (Types c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0, c t0)
  => Name Pipe
  -> Types c0 t0
  -> Result (Repr c0 t0)
  -> Pipe c '[] (Types c0 t0) Dynamic
pipe0 n (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) Nil ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy Proxy :: ioa)
   sig = Sig [] (I $ tagsSomeType ts0)
   str = Struct G.empty
  --               = Pipe desc dyn
  -- where ty      = tagsSomeType tags0
  --       desc    = Desc name sig struct (dynRep dyn) Nil tags0
  --       sig     = Sig [] (I ty)
  --       struct  = Struct graph
  --       graph   = G.vertex ty
  --       dyn     = Dynamic typeRep pipeFun
  --       pipeFun = IOA mv Proxy Proxy Proxy ::
  --                 IOA c '[] (Types c0 t0)

pipe1 ::
  forall c ioa c1 t1 c0 t0.
  ( Typeable c
  , ioa ~ IOA c '[Types c1 t1] (Types c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0, c t0
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1)
  => Name Pipe
  -> Types c1 t1 -> Types c0 t0
  -> (Repr c1 t1 -> Result (Repr c0 t0))
  -> Pipe c '[Types c1 t1] (Types c0 t0) Dynamic
pipe1 n (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) (ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy Proxy :: ioa)
   sig = Sig [I $ tagsSomeType ts1] (I $ tagsSomeType ts0)
   str = Struct G.empty
   -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)

pipe2 ::
  forall c ioa c2 t2 c1 t1 c0 t0.
  ( Typeable c
  , ioa ~ IOA c '[Types c2 t2, Types c1 t1] (Types c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0, c t0
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2)
  => Name Pipe
  -> Types c2 t2 -> Types c1 t1 -> Types c0 t0
  -> (Repr c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> Pipe c '[Types c2 t2, Types c1 t1] (Types c0 t0) Dynamic
pipe2 n (typesTags -> ts2) (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) (ts2 :* ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy Proxy :: ioa)
   sig = Sig [I $ tagsSomeType ts2, I $ tagsSomeType ts1] (I $ tagsSomeType ts0)
   str = Struct G.empty

pipe3 ::
  forall c ioa c3 t3 c2 t2 c1 t1 c0 t0.
  ( Typeable c
  , ioa ~ IOA c '[Types c3 t3, Types c2 t2, Types c1 t1] (Types c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0, c t0
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c3, ReifyVTag t3, Typeable c3, Typeable t3)
  => Name Pipe
  -> Types c3 t3 -> Types c2 t2 -> Types c1 t1 -> Types c0 t0
  -> (Repr c3 t3 -> Repr  c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> Pipe c '[Types c3 t3, Types c2 t2, Types c1 t1] (Types c0 t0) Dynamic
pipe3 n (typesTags -> ts3) (typesTags -> ts2) (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) (ts3 :* ts2 :* ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy Proxy :: ioa)
   sig = Sig [ I $ tagsSomeType ts3
             , I $ tagsSomeType ts2
             , I $ tagsSomeType ts1
             ] (I $ tagsSomeType ts0)
   str = Struct G.empty

pipe0G ::
  forall c0 t0.
  (ReifyCTag c0, ReifyVTag t0, Typeable c0, Ground t0)
  => Name Pipe
  -> Types c0 t0
  -> Result (Repr c0 t0)
  -> SomePipe Dynamic
pipe0G n t0 pf = G mempty $ pipe0 n t0 pf

pipe0T ::
  forall c0 t0.
  (ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Types c0 t0
  -> Result (Repr c0 t0)
  -> SomePipe Dynamic
pipe0T n t0 pf = T mempty $ pipe0 n t0 pf

pipe1G ::
  forall c1 t1 c0 t0.
  ( ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Ground t0)
  => Name Pipe
  -> Types c1 t1 -> Types c0 t0
  -> (Repr c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
pipe1G n t1 t0 pf = G mempty $ pipe1 n t1 t0 pf

pipe1T ::
  forall c1 t1 c0 t0.
  ( ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Types c1 t1 -> Types c0 t0
  -> (Repr c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
pipe1T n t1 t0 pf = T mempty $ pipe1 n t1 t0 pf

pipe2G ::
  forall c2 t2 c1 t1 c0 t0.
  ( ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Ground t0)
  => Name Pipe
  -> Types c2 t2 -> Types c1 t1 -> Types c0 t0
  -> (Repr c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
pipe2G n t2 t1 t0 pf = G mempty $ pipe2 n t2 t1 t0 pf

pipe2T ::
  forall c2 t2 c1 t1 c0 t0.
  ( ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Types c2 t2 -> Types c1 t1 -> Types c0 t0
  -> (Repr c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
pipe2T n t2 t1 t0 pf = T mempty $ pipe2 n t2 t1 t0 pf

pipe3G ::
  forall c3 t3 c2 t2 c1 t1 c0 t0.
  ( ReifyCTag c3, ReifyVTag t3, Typeable c3, Typeable t3
  , ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Ground t0)
  => Name Pipe
  -> Types c3 t3 -> Types c2 t2 -> Types c1 t1 -> Types c0 t0
  -> (Repr c3 t3 -> Repr  c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
pipe3G n t3 t2 t1 t0 pf = G mempty $ pipe3 n t3 t2 t1 t0 pf

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
