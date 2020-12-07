{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Orphanage (failRead, failRead') where

import Algebra.Graph
import Algebra.Graph.AdjacencyMap
import Codec.Serialise
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Map.Monoidal.Strict hiding (fromList, toList)
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Set.Monad as Set
import Generics.SOP
import Text.Read
import Type.Reflection


instance Serialise a => Serialise (I a) where
  encode (I x) = encode x
  decode = I <$> decode

instance Serialise a =>  Serialise (Interval a)

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

failRead' :: String -> ReadPrec a
failRead' desc = error $ "Cannot read instances of "<>desc<>".  Sorry."

instance Read SomeTypeRep where readPrec = failRead

