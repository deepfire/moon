module Dom.SomePipe.SomePipe (module Dom.SomePipe.SomePipe) where

import Basis

import Dom.CTag
import Dom.Cap
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Sig
import Dom.Struct


--------------------------------------------------------------------------------
-- * Key types
--
-- | A wrapped cartesian product of all possible kinds of 'Pipe's:
--   - wire-transportable types (constrained 'Ground') vs. 'Top'-(un-)constrained
--   - saturated vs. unsaturated.
data SomePipe (p :: *)
  = forall (l :: Liveness) (cas :: [*]) (o :: *). (PipeConstr l cas o) =>
    SP
    { spQName :: !(QName Pipe)
    , spCaps  :: !(Caps (CTagVV o))
    , spPipe  :: !(Pipe (l :: Liveness) (cas :: [*]) (o :: *) (p :: *))
    }

pattern SPipeD :: Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe p
pattern SPipeD name sig str rep <- SP _ _caps (PipeD name sig str rep _ _ _ _)

--------------------------------------------------------------------------------
-- * Instances
--
-- XXX: We risk equating different pipes with same names and types.
instance Eq (SomePipe p) where
  l' == r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      (pdRep    l  == pdRep      r) &&
      (pdName   l  == pdName     r) &&
      (pdStruct l  == pdStruct   r)

-- XXX: We risk equating different pipes with same names and types.
instance Ord (SomePipe p) where
  l' `compare` r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      pdRep l `compare` pdRep    r

instance Read (SomePipe ()) where
  readPrec = failRead

instance Functor SomePipe where
  fmap f (SP h c x) = SP h c (f <$> x)

instance Foldable SomePipe where
  foldMap toM = \case
    SP _ _ (Pipe _ x) -> toM x

instance Traversable SomePipe where
  traverse f = \case
    SP n c (Pipe d x) -> f x <&> \x' -> SP n c (Pipe d x')

instance Show (SomePipe p) where
  show (SP _ _ p) = "SomePipe "<>unpack (showPipe p)

--------------------------------------------------------------------------------
-- * Key tools
--
withSomePipe
  :: forall (p :: *) (a :: *)
   . SomePipe p
  -> (forall l (cas :: [*]) (o :: *)
      . (PipeConstr l cas o)
      => Pipe l cas  o  p -> a)
  -> a
withSomePipe SP{..} = ($ spPipe)
