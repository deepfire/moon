{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Orphanage (failRead) where

import Algebra.Graph
import Codec.Serialise
import Data.Set.Monad
import GHC.Generics
import Text.Read
import Type.Reflection

deriving instance Generic (Graph a)
instance Serialise a => Serialise (Graph a) where

instance (Ord a, Serialise a) => Serialise (Set a) where
  decode = fromList <$> decode
  encode x = encode (toList x)

failRead :: forall a. Typeable a => ReadPrec a
failRead = error $ "Cannot read instances of "<>show (typeRep @a)<>".  Sorry."

instance Read SomeTypeRep where readPrec = failRead

