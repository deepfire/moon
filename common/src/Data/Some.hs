{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Data.Some
  (Some(..), Some2(..))
where

import Data.Kind (Constraint, Type)


data Some  :: (k -> *) -> * where
  Exists   :: forall c f x.   f x   -> Some f

data Some2 :: (k1 -> k2 -> *) -> * where
  Exists2  :: forall c f x y. f x y -> Some2 f
