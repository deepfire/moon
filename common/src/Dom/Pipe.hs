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
data Pipe (as :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr as o =>
    { pDesc :: Desc as o
    , p     :: p
    } -> Pipe as o p

-- | Everything there is to be said about a pipe,
--   except for its representation.
data Desc (cas :: [*]) (o :: *) =
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
pattern PipeD :: ( ArgConstr o
                 , All Typeable cas
                 , All IsCTagV (cas :: [*])
                 )
              => Name Pipe -> ISig -> Struct -> SomeTypeRep
              -> NP Tags cas
              -> Tags o
              -> p
              -> Pipe cas o p
-- TODO:  get rid of this, the added benefit is too small.
pattern PipeD name sig str rep args out p
              <- Pipe (Desc name sig str rep args out) p

--------------------------------------------------------------------------------
-- * Instances
--
deriving instance Eq  p => Eq  (Pipe as o p)
deriving instance Ord p => Ord (Pipe as o p)

instance Functor (Pipe as o) where
  fmap f (Pipe d x) = Pipe d (f x)

instance Show (Pipe as o p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show (Desc as o) where show = unpack . showDesc
instance (Typeable as, Typeable o) => Read (Desc as o) where readPrec = failRead

instance Eq (Desc as o) where
  -- XXX: slightly opportustic, due to omissions.
  Desc ln _ lst lrep _ _ == Desc rn _ rst rrep _ _ =
    ln == rn && lst == rst && lrep == rrep

instance Ord (Desc as o) where
  Desc ln _ lst lrep _ _ `compare` Desc rn _ rst rrep _ _ =
    ln `compare` rn <> lrep `compare` rrep <> lst `compare` rst

instance NFData (Desc as o) where
  rnf (Desc x y z w _ u) = -- XXX: ugh
    rnf x `seq` rnf y `seq` rnf z `seq` rnf w `seq` rnf u

--------------------------------------------------------------------------------
-- * Pipe utils
--
pipeName :: (PipeConstr as o) => Pipe as o p -> Name Pipe
pipeName (PipeD name _ _ _ _ _ _)   = name
pipeName _ = error "impossible pipeName"

pipeSig :: (PipeConstr as o) => Pipe as o p -> ISig
pipeSig   (PipeD _ sig _ _ _ _ _)    = sig
pipeSig _ = error "impossible pipeSig"

pipeStruct :: (PipeConstr as o) => Pipe as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _) = struct
pipeStruct _ = error "impossible pipeStruct"

pipeRep :: (PipeConstr as o) => Pipe as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _)    = rep
pipeRep _ = error "impossible pipeRep"

pipeArityCase
  :: forall (as :: [*]) (o :: *) (p :: *) (r :: *)
  .  (PipeConstr as o)
  => Pipe as o p
  -- Zero.
  -> (forall zas' o'
      . (zas' ~ '[], PipeConstr zas' o')
      => Pipe zas' o' p -> r)
  -- One.
  -> (forall (a :: *) (sas' :: [*]) o'
      . (sas' ~ (a:'[]), PipeConstr '[] o', PipeConstr sas' o'
        , IsCTagV a, IsCTagV (Head sas'))
      => Pipe sas' o' p -> r)
  -- Infinity.
  -> (forall (a :: *) (nas' :: [*]) (as'' :: [*]) o'
      . (nas' ~ (a:as''), PipeConstr as'' o', PipeConstr nas' o')
      => Pipe nas' o' p -> r)
  -> r
pipeArityCase p@(Pipe Desc {pdArgs =      Nil} _) z _ _ = z p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* Nil} _) _ s _ = s p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* _}   _) _ _ n = n p

withCompatiblePipes
  :: forall as1 as2 o1 o2 p a
  .  ( Typeable as1, Typeable as2
     , Typeable o1, Typeable o2)
  => (forall as o. Pipe as o p -> Pipe as o p -> a)
  -> Pipe as1 o1 p
  -> Pipe as2 o2 p
  -> Maybe a
withCompatiblePipes f l r
  | Just HRefl <- typeRep @as1 `eqTypeRep` typeRep @as2
  , Just HRefl <- typeRep @o1  `eqTypeRep` typeRep @o2
  = Just $ f l r
  | otherwise = Nothing

showPipe, showPipeP :: Pipe as o p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc

pipeOutSomeCTagType ::
  forall as o co to p
  . ( PipeConstr as o
    , ReifyCTag co
    , o ~ CTagV co to)
  => Pipe as o p -> (SomeCTag, SomeTypeRep)
pipeOutSomeCTagType PipeD{} =
  ( SomeCTag $ reifyCTag $ Proxy @co
  , someTypeRep $ Proxy @to)
pipeOutSomeCTagType _ = error "pipeOutSomeCTagType: impossible"

--------------------------------------------------------------------------------
-- * Desc utils
--
descOutCTag :: Desc cas (CTagV to o) -> CTag to
descOutCTag = tCTag . pdOut

descOutVTag :: Desc cas (CTagV to o) -> VTag o
descOutVTag = tVTag . pdOut

showDesc, showDescP :: Desc as o -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>unpack (showSig $ pdSig p)
showDescP = ("("<>) . (<>")") . showDesc
