module Pipe.Types
  ( SomePipeSpace
  , PipeSpace(..)
  , SomePipeScope
  , Ops(..)
  , PipeOps(..)
  , SomePipe(..)
  , somePipeName
  , somePipeSig
  , withSomePipe
  , ArgConstr
  , ArgsConstr
  , PipeConstr
  , PipeFunTy
  -- , PipePairFunTy
  , Result
  , Pipe(..)
  , pipeSig
  , pipeName
  , pipeStruct
  , pipeRep
  , showPipe
  , showPipeP
  , Desc(..)
  , showDesc
  , pattern PipeD
  , Sig(..)
  , showSig
  , Struct(..)
  , Value(..)
  , withCompatiblePipes
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Codec.CBOR.Encoding                (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (decodeListLen, decodeWord)
import           Data.Dynamic
import qualified Data.Text                        as T
import           Type.Reflection                    ((:~~:)(..), eqTypeRep)
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import           GHC.Generics                       (Generic)

import Basis
import Type
import Namespace (Space, PointScope, spaceEntries)


--------------------------------------------------------------------------------
-- * Key types
--

-- | Pipe: component of a computation.  Characterised by:
--   - 'c'  -- either 'Ground' (wire-transportable) or 'Top' (not so)
--   - 'as' -- a type-level list of its argument specs, (TypeSpec (Type k a))
--   - 'o'  -- output type, a single 'TypeSpec' type.
data Pipe (c :: * -> Constraint) (kas :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr c kas o =>
    { pDesc :: Desc c kas o
    , p     :: p
    } -> Pipe c kas o p

-- | A wrapped cartesian product of all possible kinds of 'Pipe's:
--   - wire-transportable types (constrained 'Ground') vs. 'Top'-(un-)constrained
--   - saturated vs. unsaturated.
data SomePipe p
  = forall o
    .     (PipeConstr  Ground '[] o)
    => GS (Pipe        Ground '[] o p)
  | forall o
    .     (PipeConstr  Top    '[] o)
    => TS (Pipe        Top    '[] o p)
  | forall kas o k a kas'
    .     (PipeConstr  Ground kas o
          , kas ~ (Type k a : kas')
          )
    => G  (Pipe        Ground kas o p)
  | forall kas o k a kas'
    .     (PipeConstr  Top    kas o
          , kas ~ (Type k a : kas')
          )
    => T  (Pipe        Top    kas o p)

withSomePipe :: SomePipe p -> (forall c kas o. Pipe c kas o p -> a) -> a
withSomePipe (GS x) f = f x
withSomePipe (TS x) f = f x
withSomePipe (G  x) f = f x
withSomePipe (T  x) f = f x

type ArgConstrNC   ka    = (Typeable ka, Typeable (TagOf ka), Typeable (TypeOf ka))
type ArgConstr   c ka    = (ArgConstrNC ka,  Typeable c, c (TypeOf ka))

type ArgsConstrNC  kas   = (All Typeable kas, Typeable kas)
type ArgsConstr  c kas   = (ArgsConstrNC kas, Typeable c, All c (kas :: [*]))

type PipeConstr  c kas o = (ArgsConstr Top kas, ArgConstr c o)

-- | Result of running a pipe.
type Result a = IO (Either Text a)

type family PipeFunTy (as :: [*]) (o :: *) :: * where
  PipeFunTy '[]    o = Result (ReprOf o)
  PipeFunTy (x:xs) o = ReprOf x -> PipeFunTy xs o

-- | Everything there is to be said about a pipe, except for its function.
data Desc (c :: * -> Constraint) (kas :: [*]) (o :: *) =
  Desc
  { pdName   :: !(Name Pipe)
  , pdSig    :: !Sig
  , pdStruct :: !Struct
  , pdRep    :: !SomeTypeRep -- ^ Full type of the pipe, App4-style.
  , pdArgs   :: !(NP TypePair kas)
  , pdOut    :: !(TypePair o)
  }
  deriving (Generic)

-- | Sig:  serialisable type signature
data Sig =
  Sig
  { sArgs :: [SomeType]
  , sOut  :: SomeType
  }
  deriving (Eq, Generic, Ord)

-- | Struct: Pipe's internal structure, as a graph of type transformations.
newtype Struct = Struct (G.Graph SomeType) deriving (Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------
-- * PipeSpace -- move to Pipe.Space?
--
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
      :: forall c as o. (PipeConstr c as o)
      => Desc c as o -> Value (Arg1Tag as) (Arg1Ty as) -> p -> p
    , comp
      -- :: forall c1 c2   k t kt1 tt1
      --        kf2 tf2
      -- . ( Typeable c1, Typeable c2
      --   , Typeable kf2, Typeable tf2, Typeable k, Typeable t, Typeable kt1, Typeable tt1
      --   , ReifyTag k, ReifyTag kt1
      --   , c1 (), c1 tt1)
      -- => Desc c2 kf2 tf2 k t -> p
      -- -> Desc c1         k t kt1 tt1 -> p
      :: forall c1 c2 as1 as2 o1 o2
      . ( PipeConstr c1 as1 o1, PipeConstr c2 as2 o2
        , o2 ~ Arg1 as1
        )
      => Desc c2 as2 o2 -> p
      -> Desc c1 as1 o1 -> p
      -> p
    , trav
      -- :: forall c1 c2 t tt1 kf2 tf2 kt2
      --     . ( Typeable c1, Typeable c2, c1 (), c1 tt1
      --       , Typeable t, Typeable tt1, Typeable kf2, Typeable tf2, Typeable kt2
      --       , ReifyTag kf2, ReifyTag kt2)
      --     => Desc c1         Point t Point tt1 -> p
      --     -> Desc c2 kf2 tf2 kt2   t           -> p
      :: forall c1 c2 as1 as2 o1 o2
      . ( PipeConstr c1 as1 o1, PipeConstr c2 as2 o2
        , Arg1Tag as1 ~ Point
        , TagOf o1 ~ Point
        , Arg1Ty ty1 ~ TypeOf o2
        )
      => Desc c1 as1 o1 -> p
      -> Desc c2 as2 o2 -> p
      -> p
    } -> Ops p

class PipeOps p where
  pipeOps   :: Ops p

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * SomePipe
--
instance Functor SomePipe where
  fmap f (GS x) = GS (f <$> x)
  fmap f (TS x) = TS (f <$> x)
  fmap f (G  x) = G  (f <$> x)
  fmap f (T  x) = T  (f <$> x)

somePipeName :: SomePipe p -> Name Pipe
somePipeName (GPipeD name _ _ _) = coerceName name
somePipeName (TPipeD name _ _ _) = coerceName name

somePipeSig :: SomePipe p -> Sig
somePipeSig  (GPipeD _ sig _ _) = sig
somePipeSig  (TPipeD _ sig _ _) = sig

instance Read (SomePipe ()) where
  readPrec = failRead

--------------------------------------------------------------------------------
-- * Pipe
--
-- XXX: potentially problematic instance
instance Eq (Pipe c as o ()) where
  (==) = (==) `on` pDesc

-- XXX: potentially problematic instance
instance Ord (Pipe c as o ()) where
  compare = compare `on` pDesc

-- XXX: potentially problematic instance
instance Eq (SomePipe ()) where
  l' == r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      (pdRep    l  == pdRep      r) &&
      (pdName   l  == pdName     r) &&
      (pdStruct l  == pdStruct   r)

-- XXX: potentially problematic instance
instance Ord (SomePipe ()) where
  l' `compare` r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      (pdRep l `compare` pdRep    r)


-- * SomePipe ()
--
withCompatiblePipes
  :: forall c1 c2 as1 as2 o1 o2 p a
  .  (PipeConstr c1 as1 o1, PipeConstr c2 as2 o2)
  => (forall c as o. Pipe c as o p -> Pipe c as o p -> a)
  -> Pipe c1 as1 o1 p
  -> Pipe c2 as2 o2 p
  -> Maybe a
withCompatiblePipes f l r
  | Just HRefl <- typeRep @as1 `eqTypeRep` typeRep @as2
  , Just HRefl <- typeRep @o1  `eqTypeRep` typeRep @o2
  , Just HRefl <- typeRep @c1  `eqTypeRep` typeRep @c2
  = Just $ f l r
  | otherwise = Nothing

instance Functor (Pipe c as o) where
  fmap f (Pipe d x) = Pipe d (f x)

pattern PipeD :: (PipeConstr c as o)
              => Name Pipe -> Sig -> Struct -> SomeTypeRep
              -> NP TypePair as
              -> TypePair o
              -> p
              -> Pipe c as o p
pattern PipeD name sig str rep args out p
              <- Pipe (Desc name sig str rep args out) p

pattern GPipeD, TPipeD :: Name Pipe -> Sig -> Struct -> SomeTypeRep -> SomePipe p
pattern GPipeD name sig str rep <- G (PipeD name sig str rep _ _ _)
pattern TPipeD name sig str rep <- T (PipeD name sig str rep _ _ _)


-- * Desc
--
instance Eq (Desc c as o) where
  -- XXX: slightly opportustic, due to omissions.
  Desc ln _ lst lrep _ _ == Desc rn _ rst rrep _ _ =
    ln == rn && lst == rst && lrep == rrep

instance Ord (Desc c as o) where
  Desc ln _ lst lrep _ _ `compare` Desc rn _ rst rrep _ _ =
    ln `compare` rn <> lrep `compare` rrep <> lst `compare` rst

instance NFData (Name a)
instance NFData (QName a)
instance NFData SomeType
instance NFData Sig
instance NFData (Desc c as o) where
  rnf (Desc x y z w _ u) = -- XXX: ugh
    rnf x `seq` rnf y `seq` rnf z `seq` rnf w `seq` rnf u
instance NFData Struct

instance Show (Desc c as o) where show = unpack . showDesc

showDesc, showDescP :: Desc c as o -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>show (pdSig p)
showDescP = ("("<>) . (<>")") . showDesc

pipeName :: (PipeConstr c as o)
         => Pipe c as o p -> Name Pipe
pipeName   (PipeD name _ _ _ _ _ _)   = name

pipeSig :: (PipeConstr c as o)
        => Pipe c as o p -> Sig
pipeSig   (PipeD _ sig _ _ _ _ _)    = sig

pipeStruct :: (PipeConstr c as o)
           => Pipe c as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _) = struct

pipeRep :: (PipeConstr c as o)
        => Pipe c as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _)    = rep

showPipe, showPipeP :: Pipe c as o p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc


-- * Sig
--
showSig :: Sig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSig (Sig as o) = T.intercalate " ⇨ " $ showSomeType <$> (as <> [o])

--------------------------------------------------------------------------------
{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show (SomePipe p) where
  show (G p) = "GPipe "<>unpack (showPipe p)
  show (T p) = "TPipe "<>unpack (showPipe p)

instance Show (Pipe c as o p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show Sig where
  show x  =  "("<>T.unpack (showSig x)<>")"
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead

instance (Typeable c, Typeable as, Typeable o) => Read (Desc c as o) where readPrec = failRead
