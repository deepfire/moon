module Dom.Cap (module Dom.Cap) where

import Data.Dependent.Map     qualified as DMap
import Data.Dependent.Sum
import Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import Data.Kind                          (Constraint, Type)
import Data.Typeable                      (Typeable, (:~:)(..))

import Dom.Ground

import Data.Shelf
import Data.TyDict


--------------------------------------------------------------------------------
-- * Capability index
--
data Cap (c :: Type -> Constraint) where
  CGround   :: Cap Ground
  CShow     :: Cap Show
  CTypeable :: Cap Typeable

instance GEq Cap where
  geq a b = case (a,b) of
    (,) CGround   CGround   -> Just Refl
    (,) CShow     CShow     -> Just Refl
    (,) CTypeable CTypeable -> Just Refl
    _ -> Nothing

instance GCompare Cap where
  gcompare a b = case geq a b of
    Just Refl -> GEQ
    Nothing -> case orderCap a `compare` orderCap b of
      LT -> GLT
      GT -> GGT
   where
     orderCap :: forall c. Cap c -> Int
     orderCap = \case
       CGround   -> 0
       CShow     -> 1
       CTypeable -> 2

type Caps = Shelf Cap

capsT :: Typeable a => Caps a
capsT =
  Shelf (DMap.singleton CTypeable Dict)

capsTS :: (Typeable a, Show a) => Caps a
capsTS =
  Shelf (DMap.fromList [ CShow     :=> Dict
                       , CTypeable :=> Dict])

capsTSG :: (Typeable a, Show a, Ground a) => Caps a
capsTSG =
  Shelf (DMap.fromList [ CGround   :=> Dict
                       , CShow     :=> Dict
                       , CTypeable :=> Dict])
