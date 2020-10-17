module Dom.Some (module Dom.Some) where

import Data.Kind (Constraint)


--------------------------------------------------------------------------------
-- | Some:  abstract over a (possibly constrained) type variable.
--
data Some :: (tag -> *) -> * where
  Exists :: forall f x. f x -> Some f

data CSome :: (tag -> Constraint) -> (tag -> *) -> * where
  CSome :: forall c f x. c x => f x -> CSome c f

mkCSome :: c a => tag a -> CSome c tag
mkCSome = CSome

withCSome :: CSome c tag -> (forall a. c a => tag a -> b) -> b
withCSome (CSome x) f = f x

mapCSome :: forall c f g. (forall t. c t => f t -> g t) -> CSome c f -> CSome c g
mapCSome f (CSome x) = CSome (f x)
