module Dom.SomeVTag (module Dom.SomeVTag) where

import           Data.Typeable                      (Typeable)
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
  SomeVTag (_ :: VTag a) == SomeVTag (_ :: VTag b) =
    case (withRepGround (SomeTypeRep $ typeRep @a) (\x _ _ _-> x),
          withRepGround (SomeTypeRep $ typeRep @b) (\x _ _ _-> x)) of
      (Just ixA, Just ixB) -> ixA == ixB
      _ -> False

instance Ord SomeVTag where
  SomeVTag (_ :: VTag a) `compare` SomeVTag (_ :: VTag b) =
    case (withRepGround (SomeTypeRep $ typeRep @a) (\x _ _ _-> x),
          withRepGround (SomeTypeRep $ typeRep @b) (\x _ _ _-> x)) of
      (Just ixA, Just ixB) -> ixA `compare` ixB
      (Just _,  Nothing) -> GT
      (Nothing, Just _)  -> LT
      _ -> LT
