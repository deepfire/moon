{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Orphanage (failRead) where

import Algebra.Graph
import Algebra.Graph.AdjacencyMap
import Codec.Serialise
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Set.Monad as Set
import GHC.Generics
import Text.Read
import Type.Reflection

import Data.Map.Monoidal.Strict hiding (fromList, toList)

instance (Ord k, Serialise k, Semigroup v, Serialise v) => Serialise (MonoidalMap k v) where
  encode = encode . MMap.toList
  decode = MMap.fromList <$> decode

instance Serialise a => Serialise (Graph a)
instance (Ord a, Serialise a) => Serialise (AdjacencyMap a)

instance (Ord a, Serialise a) => Serialise (Set.Set a) where
  decode = Set.fromList <$> decode
  encode x = encode (Set.toList x)

failRead :: forall a. Typeable a => ReadPrec a
failRead = error $ "Cannot read instances of "<>show (typeRep @a)<>".  Sorry."

instance Read SomeTypeRep where readPrec = failRead

