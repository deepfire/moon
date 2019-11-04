module Pipe.Types
  ( SomePipeSpace
  , PipeSpace(..)
  , SomePipeScope
  , Ops(..)
  , PipeOps(..)
  , SomePipe(..)
  , somePipeDesc
  , somePipeName
  , somePipeSig
  , Pipe(..)
  , pipeSig
  , pipeName
  , pipeStruct
  , pipeRep
  , showPipe
  , showPipeP
  , SomeDesc(..)
  , someDescName
  , someDescSig
  , Desc(..)
  , showDesc
  , pattern PipeD
  , Sig(..)
  , showSig
  , Struct(..)
  , Result
  , Value(..)
  , withCompatiblePipes
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Codec.CBOR.Encoding                (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (decodeListLen, decodeWord)
import           Data.Dynamic
import           Type.Reflection                    ((:~~:)(..), eqTypeRep)
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import           GHC.Generics                       (Generic)

import Basis
import Type
import Namespace (Space, PointScope, spaceEntries)


--------------------------------------------------------------------------------
type SomePipeSpace p = PipeSpace (SomePipe p)

data PipeSpace a = PipeSpace
  { psName  :: !(QName (PipeSpace))
  , psSpace :: !(Space Point a)
  , psFrom  :: !(MonoidalMap SomeTypeRep (Set (QName Pipe)))
  , psTo    :: !(MonoidalMap SomeTypeRep (Set (QName Pipe)))
  } deriving (Eq, Ord)

instance Functor PipeSpace where
  fmap f ps@PipeSpace{psSpace} =
    ps { psSpace = f <$> psSpace }

instance (Ord a, Serialise a, Typeable a)
       => Serialise (PipeSpace a) where
  encode PipeSpace{psName, psSpace, psFrom, psTo} =
    encodeListLen 5
    <> encodeWord 2177
    <> encode psName
    <> encode psSpace
    <> encode (MMap.map setToList psFrom)
    <> encode (MMap.map setToList psTo)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (5, 2177) -> do
        PipeSpace
        <$> decode
        <*> decode
        <*> (MMap.map Set.fromList <$> decode)
        <*> (MMap.map Set.fromList <$> decode)
      _ -> fail $ "PipeSpace failed decode: "
           <>" len="<> show len
           <>" tag="<> show tag
           <>" rep="<> show (typeRep @(PipeSpace a))

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

type SomePipeScope p = PointScope (SomePipe p)

--------------------------------------------------------------------------------
-- * Operations
--
data Ops p where
  Ops ::
    { app
      :: forall c kf tf kt tt
      . ( Typeable c, Typeable kf, Typeable tf, Typeable kt, Typeable tt
        , ReifyTag kt
        , c tt)
      => Desc c kf tf kt tt -> Value kf tf -> p -> p
    , comp
      :: forall c1 c2   k t kt1 tt1
             kf2 tf2
      . ( Typeable c1, Typeable c2
        , Typeable kf2, Typeable tf2, Typeable k, Typeable t, Typeable kt1, Typeable tt1
        , ReifyTag k, ReifyTag kt1
        , c1 (), c1 tt1)
      => Desc c2 kf2 tf2 k t -> p
      -> Desc c1         k t kt1 tt1 -> p
      -> p
    , trav
      :: forall c1 c2 t tt1 kf2 tf2 kt2
          . ( Typeable c1, Typeable c2, c1 (), c1 tt1
            , Typeable t, Typeable tt1, Typeable kf2, Typeable tf2, Typeable kt2
            , ReifyTag kf2, ReifyTag kt2)
          => Desc c1         Point t Point tt1 -> p
          -> Desc c2 kf2 tf2 kt2   t           -> p
          -> p
    } -> Ops p

class PipeOps p where
  pipeOps   :: Ops p

--------------------------------------------------------------------------------
data SomePipe p
  = forall c ka a kb b. ( c ~ Ground, c b
                        , ReifyTag ka, ReifyTag kb
                        , Typeable ka, Typeable  a, Typeable kb, Typeable  b)
    => G { unSomePipe :: (Pipe c ka a kb b p) }
  | forall c ka a kb b. ( c ~ Top
                        , ReifyTag ka, ReifyTag kb
                        , Typeable ka, Typeable  a, Typeable kb, Typeable  b)
    => T { unSomePipe :: (Pipe c ka a kb b p) }

instance Functor SomePipe where
  fmap f (G x) = G (f <$> x)
  fmap f (T x) = T (f <$> x)

somePipeDesc :: SomePipe p  -> SomeDesc
somePipeDesc (G Pipe{pDesc}) = SomeDesc pDesc
somePipeDesc (T Pipe{pDesc}) = SomeDesc pDesc

somePipeName :: SomePipe p -> Name Pipe
somePipeName (GPipeD name _ _ _) = coerceName name
somePipeName (TPipeD name _ _ _) = coerceName name

somePipeSig :: SomePipe p -> Sig
somePipeSig  (GPipeD _ sig _ _) = sig
somePipeSig  (TPipeD _ sig _ _) = sig

-- | Pipe: component of a computation
data Pipe (c :: * -> Constraint) (ka :: Con) (a :: *) (kb :: Con) (b :: *) (p :: *) where
  Pipe :: (ReifyTag ka, ReifyTag kb, Typeable kb, c b) =>
    { pDesc :: Desc c ka a kb b
    , p     :: p
    } -> Pipe c ka a kb b p

-- XXX: potentially problematic instance
instance Eq (Pipe c ka a kb b ()) where
  (==) = (==) `on` pDesc

-- XXX: potentially problematic instance
instance Ord (Pipe c ka a kb b ()) where
  compare = compare `on` pDesc

-- XXX: potentially problematic instance
instance Eq (SomePipe ()) where
  l == r = ((==) `on` somePipeDesc) l r

-- XXX: potentially problematic instance
instance Ord (SomePipe ()) where
  l `compare` r = (compare `on` somePipeDesc) l r

withCompatiblePipes
  :: forall c1 c2 kf1 tf1 kt1 tt1 kf2 tf2 kt2 tt2 p a
  .  ( Typeable c1, Typeable c2
     , Typeable kf1, Typeable tf1, Typeable kt1, Typeable tt1
     , Typeable kf2, Typeable tf2, Typeable kt2, Typeable tt2)
  => (forall c kf tf kt tt
      . Pipe c kf tf kt tt p -> Pipe c kf tf kt tt p -> a)
  -> Pipe c1 kf1 tf1 kt1 tt1 p
  -> Pipe c2 kf2 tf2 kt2 tt2 p
  -> Maybe a
withCompatiblePipes f l r
  | Just HRefl <- typeRep @tf1 `eqTypeRep` typeRep @tf2
  , Just HRefl <- typeRep @tt1 `eqTypeRep` typeRep @tt2
  , Just HRefl <- typeRep @kf1 `eqTypeRep` typeRep @kf2
  , Just HRefl <- typeRep @kt1 `eqTypeRep` typeRep @kt2
  , Just HRefl <- typeRep @c1  `eqTypeRep` typeRep @c2
  = Just $ f l r
  | otherwise = Nothing

instance Functor (Pipe c ka a kb b) where
  fmap f (Pipe d x) = Pipe d (f x)

pattern PipeD :: (ReifyTag ka, ReifyTag kb)
              => Name Pipe -> Sig -> Struct -> SomeTypeRep
              -> Tag ka -> Proxy a -> Tag kb -> Proxy b -> p
              -> Pipe c ka a kb b p
pattern PipeD name sig str rep ka a kb b p
              <- Pipe (Desc name sig str rep ka a kb b) p

pattern GPipeD, TPipeD :: Name Pipe -> Sig -> Struct -> SomeTypeRep -> SomePipe p
pattern GPipeD name sig str rep <- G (PipeD name sig str rep _ _ _ _ _)
pattern TPipeD name sig str rep <- T (PipeD name sig str rep _ _ _ _ _)

data SomeDesc where
  SomeDesc
    :: forall c ka a kb b
    . ( Typeable c
      , ReifyTag ka, ReifyTag kb
      , Typeable ka, Typeable  a, Typeable kb, Typeable  b
      , c b
      ) =>
    { _spdDesc :: Desc c ka a kb b
    } -> SomeDesc

someDescName :: SomeDesc -> Name Pipe
someDescName (SomeDesc pd) = pdName pd

someDescSig :: SomeDesc -> Sig
someDescSig (SomeDesc pd) = pdSig pd

instance Eq SomeDesc where
  -- Note, that this is sligtly weak:
  -- we can still have effectively different pipes with the same:
  -- 1. same name (but e.g. in different namespaces)
  -- 2. structure & types
  SomeDesc l == SomeDesc r =
    (pdRep    l == pdRep    r) &&
    (pdName   l == pdName   r) &&
    (pdStruct l == pdStruct r)

instance NFData SomeDesc where
  rnf (SomeDesc x) = rnf x

instance Ord SomeDesc where
  SomeDesc l `compare` SomeDesc r =
    (pdRep l `compare` pdRep    r)

instance Read SomeDesc where
  readPrec = failRead

instance Show SomeDesc where
  show (SomeDesc pd) = show pd

data Desc (c :: * -> Constraint) (ka :: Con) (a :: *) (kb :: Con) (b :: *) = Desc
  { pdName   :: !(Name Pipe)
  , pdSig    :: !Sig
  , pdStruct :: !Struct
  , pdRep    :: !SomeTypeRep -- ^ Full type of the pipe, App4-style.
  , pdFromK  :: !(Tag  ka)
  , pdFrom   :: !(Proxy a)
  , pdToK    :: !(Tag  kb)
  , pdTo     :: !(Proxy b)
  } deriving (Eq, Generic, Ord)

instance NFData (Name a)
instance NFData (QName a)
instance NFData Type
instance NFData Sig
instance NFData (Desc c ka a kb b)
instance NFData Struct

instance Show (Desc c ka a kb b) where show = unpack . showDesc

showDesc, showDescP :: Desc c ka a kb b -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>show (pdSig p)
showDescP = ("("<>) . (<>")") . showDesc

pipeName :: (ReifyTag ka, ReifyTag kb)
         => Pipe c ka a kb b p -> Name Pipe
pipeName   (PipeD name _ _ _ _ _ _ _ _)   = name

pipeSig :: (ReifyTag ka, ReifyTag kb)
        => Pipe c ka a kb b p -> Sig
pipeSig   (PipeD _ sig _ _ _ _ _ _ _)    = sig

pipeStruct :: (ReifyTag ka, ReifyTag kb)
           => Pipe c ka a kb b p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _ _ _) = struct

pipeRep :: (ReifyTag ka, ReifyTag kb)
        => Pipe c ka a kb b p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _ _ _)    = rep

showPipe, showPipeP :: Pipe c ka a kb b p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc

showSig :: Sig -> Text
showSig Gen{sOut} = showType sOut
showSig Link{sIn, sOut} = showType sIn <> " ↦ ↣ → ⇨ ⇒ " <> showType sOut

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

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show (SomePipe p) where
  show (G p) = "GPipe "<>unpack (showPipe p)
  show (T p) = "TPipe "<>unpack (showPipe p)

instance Show (Pipe c ka a kb b p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show Sig where
  show (Gen  _ o)  =  "(Gen "<>show o<>")"
  show (Link i o)  = "(Link "<>show i<>" -> "<>show o<>")"
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead

instance (Typeable c, Typeable ka, Typeable a, Typeable kb, Typeable b) => Read (Desc c ka a kb b) where readPrec = failRead
