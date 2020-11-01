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
genPipe ::
     forall cf tf ct tt c ioa.
     ( cf ~ 'Point, tf ~ ()
     , ReifyCTag ct, ReifyVTag tt
     , Typeable ct, Typeable tt, Typeable c
     , c tt
     , ioa ~ IOA c '[] (Types ct tt))
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> Pipe c '[] (Types ct tt) Dynamic
genPipe name typ@(typesTags -> Tags cTag vTag) mv
  -- TODO: validate types against the typerep/dynamic
                = Pipe desc dyn
  where ty      = typesSomeType typ
        desc    = Desc name sig struct (dynRep dyn) Nil (Tags cTag vTag)
        sig     = Sig [] (I ty)
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mv Proxy Proxy Proxy ::
                  IOA c '[] (Types ct tt)

linkPipe ::
    forall cf tf ct tt c ioa
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable cf, Typeable ct, Typeable c
    , c tt
    , ioa ~ IOA c '[Types cf tf] (Types ct tt))
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> Pipe c '[Types cf tf] (Types ct tt) Dynamic
linkPipe name (typesTags -> tagsf) (typesTags -> tagst) mf =
  withVTag (tVTag tagsf) $ withVTag (tVTag tagst) $
    let dyn = toDyn (IOA mf Proxy Proxy Proxy :: ioa)
    in Pipe (mkDesc name tagsf tagst (dynRep dyn)) dyn

mkDesc :: ( ReifyCTag cf, ReifyCTag ct
          , Typeable cf, Typeable ct
          , Typeable tf, Typeable tt)
       => Name Pipe -> Tags (Types cf tf) -> Tags (Types ct tt)
       -> SomeTypeRep
       -> Desc c '[Types cf tf] (Types ct tt)
mkDesc name tagsf tagst pipeGutsRep =
  Desc name sig struct pipeGutsRep (tagsf :* Nil) tagst
 where
    sig = Sig [I $ tagsSomeType tagsf] (I $ tagsSomeType tagst)
    struct = Struct G.empty
    -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)

genG
  :: forall ct tt
  .  (ReifyCTag ct, ReifyVTag tt, Typeable ct, Ground tt)
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> SomePipe Dynamic
genG n to pf = G mempty $ genPipe n to pf

gen
  :: forall ct tt
  .  ( ReifyCTag ct, ReifyVTag tt
     , Typeable ct, Typeable tt)
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> SomePipe Dynamic
gen n to pf = T mempty $ genPipe n to pf

linkG
  :: forall cf tf ct tt
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable cf, Typeable tf, Typeable ct
    , Ground tt)
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> SomePipe Dynamic
linkG n from to pf = G mempty $ linkPipe n from to pf

-- linkR
--   :: forall cf tf ct tt
--   . ( ReifyCTag cf, ReifyCTag ct
--     , ReifyVTag tf, ReifyVTag tt
--     , Typeable cf, Typeable ct)
--   => Name Pipe
--   -> Types cf tf
--   -> Types ct tt
--   -> (Repr cf tf -> Result (Repr ct tt))
--   -> SomePipe Dynamic
-- linkR n from to pf = T mempty $ linkPipe n from to pf

link
  :: forall cf tf ct tt
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable cf, Typeable tf, Typeable ct
    , Typeable tt)
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> SomePipe Dynamic
link n from to pf = T mempty $ linkPipe n from to pf

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
