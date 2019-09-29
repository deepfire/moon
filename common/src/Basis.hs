module Basis
  ( module Control.Monad
  , module Data.Dynamic
  , module Data.Function
  , module Data.Kind
  , module Data.Map.Strict
  , module Data.Orphanage
  , module Data.Proxy
  , module Data.Sequence
  , module Data.Set.Monad
  , module Data.String
  , module Data.Text
  , module Debug.Trace
  , module Text.Printf
  , module Text.Read
  , module Type.Reflection
  -- * Locals
  , liftSet
  , unliftSet
  , keysSet
  , listSetUnsafe
  , setToList
  )
where

import Control.Monad         (join)
import Data.Dynamic          (Dynamic(..), Typeable)
import Data.Function         ((&))
import Data.Kind             (Constraint)
import Data.Map.Strict       (Map)
import Data.Orphanage
import Data.Proxy            (Proxy(..))
import Data.Sequence         (Seq)
import Data.Set.Monad        (Set)
import Data.String           (IsString)
import Data.Text             (Text, pack, unpack)
import Debug.Trace           (trace)
import Text.Printf           (printf)
import Text.Read             (Read(..))
import Type.Reflection       (TypeRep, SomeTypeRep, someTypeRep, typeRep)

import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad  as Set
import qualified Data.Set        as Set'

liftSet :: Ord a => Set'.Set a -> Set.Set a
liftSet = Set.fromDistinctAscList . Set'.toAscList

unliftSet :: Ord a => Set.Set a -> Set'.Set a
unliftSet = Set'.fromDistinctAscList . Set.toAscList

keysSet :: Ord k => Map k v -> Set k
keysSet = liftSet . Map.keysSet

listSetUnsafe :: Ord a => [a] -> Set.Set a
listSetUnsafe = Set.fromDistinctAscList

setToList :: Ord a => Set.Set a -> [a]
setToList = Set.toAscList
