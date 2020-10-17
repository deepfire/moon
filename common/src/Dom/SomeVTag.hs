module Dom.SomeVTag (module Dom.SomeVTag) where

import           Codec.Serialise                    (Serialise(..))
import           Control.DeepSeq                    (NFData(..))
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import           Data.Typeable                      (Proxy, Typeable, (:~:)(..), (:~~:)(..))
import           GHC.Generics                       (Generic)
import           Type.Reflection                    (SomeTypeRep(..), typeRep)

import Dom.Ground
import Dom.VTag


--------------------------------------------------------------------------------
-- * SomeVTag
--
-- Only ever touched by 'Codec.Serialise'
--
data SomeVTag where
  SomeVTag
    :: (ReifyVTag a, Typeable a)
    => VTag (a :: *) -> SomeVTag

instance Eq SomeVTag where
  SomeVTag (a :: VTag a) == SomeVTag (b :: VTag b) =
    case (withRepGround (SomeTypeRep $ typeRep @a) (\x _ _ _-> x),
          withRepGround (SomeTypeRep $ typeRep @b) (\x _ _ _-> x)) of
      (Just ixA, Just ixB) -> ixA == ixB
      _ -> False

instance Ord SomeVTag where
  SomeVTag (a :: VTag a) `compare` SomeVTag (b :: VTag b) =
    case (withRepGround (SomeTypeRep $ typeRep @a) (\x _ _ _-> x),
          withRepGround (SomeTypeRep $ typeRep @b) (\x _ _ _-> x)) of
      (Just ixA, Just ixB) -> ixA `compare` ixB
      (Just ixA, Nothing) -> GT
      (Nothing, Just ixA) -> LT
      _ -> LT
