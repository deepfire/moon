{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-orphans -Wno-unticked-promoted-constructors #-}
module Dom.Pipe (module Dom.Pipe) where

import           GHC.Generics                       (Generic)

import Basis

import Data.Orphanage ()

import Dom.CTag
import Dom.Error
import Dom.Name
import Dom.Pipe.Constr
import Dom.Sig
import Dom.Struct
import Dom.Tags
import Dom.VTag


--------------------------------------------------------------------------------
-- | Pipe: component of a computation.  Characterised by:
--   c  -- either 'Ground' (wire-transportable) or 'Top' (not so)
--   as -- a type-level list of its argument specs, (TypePair (Type c a))
--   o  -- output type, a single 'TypeSpec' type.
--   p  -- pipe's representation:
--           - Dynamic wrapping a function of matching type, or
--           - something else, perhaps simpler (like ()).
data Pipe (c :: * -> Constraint) (as :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr c as o =>
    { pDesc :: Desc c as o
    , p     :: p
    } -> Pipe c as o p

-- | Everything there is to be said about a pipe,
--   except for its representation.
data Desc (c :: * -> Constraint) (cas :: [*]) (o :: *) =
  Desc
  { pdName   :: !(Name Pipe)
  , pdSig    :: !ISig
  , pdStruct :: !Struct
  , pdRep    :: !SomeTypeRep           -- ^ Full type of the pipe.
  , pdArgs   :: !(NP Tags cas)
  , pdOut    :: !(Tags o)
  }
  deriving (Generic)

-- | Result of running a pipe.
type Result a = IO (Fallible a)

--------------------------------------------------------------------------------
-- * Destructuring
--
pattern PipeD :: ( ArgConstr c o
                 , All Typeable cas
                 , All IsCTagV cas
                 , All Top (cas :: [*])
                 )
              => Name Pipe -> ISig -> Struct -> SomeTypeRep
              -> NP Tags cas
              -> Tags o
              -> p
              -> Pipe c cas o p
-- TODO:  get rid of this, the added benefit is too small.
pattern PipeD name sig str rep args out p
              <- Pipe (Desc name sig str rep args out) p

--------------------------------------------------------------------------------
-- * Instances
--
deriving instance Eq  p => Eq  (Pipe c as o p)
deriving instance Ord p => Ord (Pipe c as o p)

instance Functor (Pipe c as o) where
  fmap f (Pipe d x) = Pipe d (f x)

instance Show (Pipe c as o p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show (Desc c as o) where show = unpack . showDesc
instance (Typeable c, Typeable as, Typeable o) => Read (Desc c as o) where readPrec = failRead

instance Eq (Desc c as o) where
  -- XXX: slightly opportustic, due to omissions.
  Desc ln _ lst lrep _ _ == Desc rn _ rst rrep _ _ =
    ln == rn && lst == rst && lrep == rrep

instance Ord (Desc c as o) where
  Desc ln _ lst lrep _ _ `compare` Desc rn _ rst rrep _ _ =
    ln `compare` rn <> lrep `compare` rrep <> lst `compare` rst

instance NFData (Desc c as o) where
  rnf (Desc x y z w _ u) = -- XXX: ugh
    rnf x `seq` rnf y `seq` rnf z `seq` rnf w `seq` rnf u

--------------------------------------------------------------------------------
-- * Pipe utils
--
pipeName :: (PipeConstr c as o) => Pipe c as o p -> Name Pipe
pipeName (PipeD name _ _ _ _ _ _)   = name
pipeName _ = error "impossible pipeName"

pipeSig :: (PipeConstr c as o) => Pipe c as o p -> ISig
pipeSig   (PipeD _ sig _ _ _ _ _)    = sig
pipeSig _ = error "impossible pipeSig"

pipeStruct :: (PipeConstr c as o) => Pipe c as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _) = struct
pipeStruct _ = error "impossible pipeStruct"

pipeRep :: (PipeConstr c as o) => Pipe c as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _)    = rep
pipeRep _ = error "impossible pipeRep"

pipeArityCase
  :: forall (c :: * -> Constraint) (as :: [*]) (o :: *) (p :: *) (r :: *)
  .  (PipeConstr c as o)
  => Pipe c as o p
  -- Zero.
  -> (forall zas' o'
      . (zas' ~ '[], PipeConstr c zas' o')
      => Pipe c zas' o' p -> r)
  -- One.
  -> (forall (a :: *) (sas' :: [*]) o'
      . (sas' ~ (a:'[]), PipeConstr c '[] o', PipeConstr c sas' o'
        , IsCTagV a, IsCTagV (Head sas'))
      => Pipe c sas' o' p -> r)
  -- Infinity.
  -> (forall (a :: *) (nas' :: [*]) (as'' :: [*]) o'
      . (nas' ~ (a:as''), PipeConstr c as'' o', PipeConstr c nas' o')
      => Pipe c nas' o' p -> r)
  -> r
pipeArityCase p@(Pipe Desc {pdArgs =      Nil} _) z _ _ = z p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* Nil} _) _ s _ = s p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* _}   _) _ _ n = n p

withCompatiblePipes
  :: forall c1 c2 as1 as2 o1 o2 p a
  .  ( Typeable as1, Typeable as2
     , Typeable o1, Typeable o2
     , Typeable c1, Typeable c2)
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

showPipe, showPipeP :: Pipe c as o p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc

pipeOutSomeCTagType ::
  forall c as o co to p
  . ( PipeConstr c as o
    , ReifyCTag co
    , o ~ CTagV co to)
  => Pipe c as o p -> (SomeCTag, SomeTypeRep)
pipeOutSomeCTagType PipeD{} =
  ( SomeCTag $ reifyCTag $ Proxy @co
  , someTypeRep $ Proxy @to)
pipeOutSomeCTagType _ = error "pipeOutSomeCTagType: impossible"

--------------------------------------------------------------------------------
-- * Desc utils
--
descOutCTag :: Desc c cas (CTagV to o) -> CTag to
descOutCTag = tCTag . pdOut

descOutVTag :: Desc c cas (CTagV to o) -> VTag o
descOutVTag = tVTag . pdOut

showDesc, showDescP :: Desc c as o -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>unpack (showSig $ pdSig p)
showDescP = ("("<>) . (<>")") . showDesc
