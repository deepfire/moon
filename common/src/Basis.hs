{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Basis
  ( module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.Bifunctor
  -- , module Data.Coerce
  , module Data.Dynamic
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Kind
  , module Data.Map.Monoidal.Strict
  , module Data.Map.Strict
  , module Data.Maybe
  , module Data.Orphanage
  , module Data.Proxy
  , module Data.Sequence
  , module Data.Set.Monad
  , module Data.String
  , module Data.Text
  , module Debug.Trace
  , module Generics.SOP
  , module Text.Printf
  , module Text.Read
  , module Type.Reflection
  -- * Locals
  , liftSet
  , unliftSet
  , keysSet
  , keysSetMMap
  , listSetUnsafe
  , setToList
  , guard
  )
where

import Control.Applicative        ((<|>), liftA2)
import Control.Arrow              ((>>>), (***), (&&&), (+++), left, right, first, second)
import Control.Monad              (join)
import Data.Bifunctor             (bimap)
import Data.Dynamic               (Dynamic(..), Typeable)
import Data.Function              ((&))
import Data.Functor               ((<&>))
import Data.Foldable              (toList)
import Data.Kind                  (Constraint)
import Data.Map.Strict            (Map)
import Data.Maybe                 (fromMaybe)
import Data.Map.Monoidal.Strict   (MonoidalMap)
import Data.Orphanage
import Data.Proxy                 (Proxy(..))
import Data.Sequence              (Seq)
import Data.Set.Monad             (Set)
import Data.String                (IsString)
import Data.Text                  (Text, pack, unpack)
import Debug.Trace                (trace)
import Generics.SOP               (All, All2, Top)
import Text.Printf                (printf)
import Text.Read                  (Read(..))
import Type.Reflection            (TypeRep, SomeTypeRep, someTypeRep, typeRep)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Set.Monad  as Set
import qualified Data.Set        as Set'

liftSet :: Ord a => Set'.Set a -> Set.Set a
liftSet = Set.fromDistinctAscList . Set'.toAscList

unliftSet :: Ord a => Set.Set a -> Set'.Set a
unliftSet = Set'.fromDistinctAscList . Set.toAscList

keysSet :: Ord k => Map k v -> Set k
keysSet = liftSet . Map.keysSet

keysSetMMap :: Ord k => MonoidalMap k v -> Set k
keysSetMMap = liftSet . MMap.keysSet

listSetUnsafe :: Ord a => [a] -> Set.Set a
listSetUnsafe = Set.fromDistinctAscList

setToList :: Ord a => Set.Set a -> [a]
setToList = Set.toAscList

guard :: e -> Maybe a -> Either e a
guard e = \case
  Nothing -> Left  e
  Just x  -> Right x
