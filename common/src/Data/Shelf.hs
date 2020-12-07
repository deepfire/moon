module Data.Shelf (module Data.Shelf) where

import Data.Dependent.Map               qualified as DMap
import Data.GADT.Compare                            (GCompare(..))
import Data.Kind                                    (Constraint)
import Data.TyDict
import Type.Reflection                              (Typeable)


newtype Shelf (cs :: (* -> Constraint) -> *) a =
  Shelf (DMap.DMap cs (Dict a))

shelfSing ::
  forall a cs c
  . (GCompare cs, c a, Typeable a)
  => cs c -> Shelf cs a
shelfSing cs = Shelf (DMap.singleton cs Dict)

addShelf ::
  forall cs c a
  . (GCompare cs, c a, Typeable a)
  => cs c -> Shelf cs a -> Shelf cs a
addShelf cs (Shelf dm) = Shelf (DMap.insert cs Dict dm)

withOpenShelf ::
  forall cs c a b
  .  GCompare cs
  => Shelf cs a
  -> cs c
  -> (c a => b)
  -> Maybe b
withOpenShelf (Shelf m) cs f =
  case DMap.lookup cs m of
    Nothing -> Nothing
    Just Dict -> Just f
