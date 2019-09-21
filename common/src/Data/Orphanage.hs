{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Orphanage () where

import Algebra.Graph
import Data.Set.Monad
import GHC.Generics
import Codec.Serialise

deriving instance Generic (Graph a)
instance Serialise a => Serialise (Graph a) where

instance (Ord a, Serialise a) => Serialise (Set a) where
  decode = fromList <$> decode
  encode x = encode (toList x)

