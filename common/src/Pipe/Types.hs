module Pipe.Types
  ( SomePipeSpace
  , PipeSpace(..)
  , SomePipeScope
  , PipeScope
  , SomePipe(..)
  , somePipeDesc
  , somePipeName
  , somePipeSig
  , Pipe(..)
  , pipeSig
  , pipeName
  , pipeStruct
  , showPipe
  , showPipeP
  , PipeDesc(..)
  , showPipeDesc
  , pattern PipeD
  , Sig(..)
  , Struct(..)
  , Result
  , pattern PGen
  , pattern PLink
  , pattern PAny
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Codec.CBOR.Encoding                (encodeListLen)
import           Codec.CBOR.Decoding                (decodeListLen)
import           Data.Dynamic
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import           GHC.Generics                       (Generic)

import Basis
import Type
import Namespace (Space, Scope, spaceEntries)


--------------------------------------------------------------------------------
type SomePipeSpace = PipeSpace SomePipe

data PipeSpace a = PipeSpace
  { psName  :: QName (PipeSpace)
  , psSpace :: !(Space Point a)
  , psFrom  :: !(MonoidalMap SomeTypeRep (Set (QName Pipe)))
  , psTo    :: !(MonoidalMap SomeTypeRep (Set (QName Pipe)))
  } deriving (Eq, Ord)

instance Functor PipeSpace where
  fmap f ps@PipeSpace{psSpace} =
    ps { psSpace = f <$> psSpace }

instance (Generic a, Ord a, Serialise a, Typeable a)
       => Serialise (PipeSpace a) where
  encode PipeSpace{psName, psSpace, psFrom, psTo} =
    encodeListLen 4
    <> encode psName
    <> encode psSpace
    <> encode (MMap.map setToList psFrom)
    <> encode (MMap.map setToList psTo)
  decode = do
    len <- decodeListLen
    case len of
      4 -> do
        PipeSpace
        <$> decode
        <*> decode
        <*> (MMap.map Set.fromList <$> decode)
        <*> (MMap.map Set.fromList <$> decode)
      _ -> fail $ "invalid "<>show (typeRep @(PipeSpace a))<>" encoding: len="<>show len

instance Typeable a => Read (PipeSpace a) where readPrec = failRead

instance Show (PipeSpace a) where
  show PipeSpace{psName, psSpace} =
    "(PipeSpace "<>show psName<>" "<>show (length $ spaceEntries psSpace)<>" entries)"

instance Semigroup (PipeSpace a) where
  l <> r = PipeSpace
    { psName  = psName  l <> psName  r
    , psSpace = psSpace l <> psSpace r
    , psFrom  = psFrom  l <> psFrom  r
    , psTo    = psTo    l <> psTo    r
    }

type PipeScope a = Scope Point a
type SomePipeScope = PipeScope SomePipe

--------------------------------------------------------------------------------
data SomePipe
  = forall c. c ~ Ground => G (Pipe c)
  | forall c. c ~ Top    => T (Pipe c)

somePipeDesc :: SomePipe -> PipeDesc
somePipeDesc (G Pipe{pipeDesc}) = pipeDesc
somePipeDesc (T Pipe{pipeDesc}) = pipeDesc

somePipeName :: SomePipe -> Name Pipe
somePipeName = coerceName . pdName . somePipeDesc

somePipeSig :: SomePipe -> Sig
somePipeSig = pdSig . somePipeDesc

-- | Pipe: a concrete, runnable 'Def'-inition.
data Pipe (c :: * -> Constraint) where
  Pipe
    :: forall (k :: Con) (a :: *) (c :: * -> Constraint)
    . (Typeable k, c a)
    =>
    { pipeDesc :: PipeDesc
    , pTag     :: Tag k
    , pTo      :: Proxy a
    , pDyn     :: Dynamic -- ^ A wrapped 'PipeFun'.
    } -> Pipe c

pattern PipeD :: Name Pipe -> Sig -> Struct -> Pipe c
pattern PipeD name sig str <- Pipe (PipeDesc name sig str) _ _ _

data PipeDesc = PipeDesc
  { pdName   :: Name Pipe
  , pdSig    :: Sig
  , pdStruct :: Struct
  } deriving (Eq, Generic, Ord)

instance Show PipeDesc where show = unpack . showPipeDesc

showPipeDesc, showPipeDescP :: PipeDesc -> Text
showPipeDesc  p = pack $ show (pdName p) <>" :: "<>show (pdSig p)
showPipeDescP = ("("<>) . (<>")") . showPipeDesc

pipeName :: Pipe c -> Name Pipe
pipeName = pdName . pipeDesc

pipeSig :: Pipe c -> Sig
pipeSig = pdSig . pipeDesc

pipeStruct :: Pipe c -> Struct
pipeStruct = pdStruct . pipeDesc

showPipe, showPipeP :: Pipe c -> Text
showPipe  = showPipeDesc  . pipeDesc
showPipeP = showPipeDescP . pipeDesc

--------------------------------------------------------------------------------
-- | Sig: a structure-oblivious abstraction of a pipe as its endpoints.
data Sig
  = Gen    -- ^  Pipe endpoint: IO a
    { sIn  :: Type
    , sOut :: Type
    }
  | Link   -- ^ Pipe transform: b → IO c
    { sIn  :: Type
    , sOut :: Type
    }
  deriving (Eq, Generic, Ord)

--------------------------------------------------------------------------------
-- | Struct: Pipe's internal structure, as a graph of type transformations.
newtype Struct = Struct (G.Graph Type) deriving (Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------
-- | Result of running a pipe.
type Result a = IO (Either Text a)

--------------------------------------------------------------------------------
pattern PGen  :: Name Pipe -> Struct -> Dynamic ->         Type -> Pipe c
pattern PGen  n st dy    so <- Pipe (PipeDesc n (Gen  _  so) st) _ _ dy

pattern PLink :: Name Pipe -> Struct -> Dynamic -> Type -> Type -> Pipe c
pattern PLink n st dy si so <- Pipe (PipeDesc n (Link si so) st) _ _ dy

pattern PAny  :: Name Pipe -> Struct -> Dynamic ->         Type -> Pipe c
pattern PAny  n st dy    so <- Pipe (PipeDesc n (sOut -> so) st) _ _ dy

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show SomePipe where
  show (G p) = "GPipe "<>unpack (showPipe p)
  show (T p) = "TPipe "<>unpack (showPipe p)

instance Show (Pipe c) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show Sig where
  show (Gen  _ o)  =  "(Gen "<>show o<>")"
  show (Link i o)  = "(Link "<>show i<>" -> "<>show o<>")"
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead

instance Serialise PipeDesc
instance Read PipeDesc where readPrec = failRead
