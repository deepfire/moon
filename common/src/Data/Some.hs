{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Data.Some
  ( Some(..), Some2(..)
  )
where

-- import Data.Kind (Constraint)


data Some  :: (k -> *) -> * where
  Exists   :: forall f x.   f x   -> Some f

data Some2 :: (k1 -> k2 -> *) -> * where
  Exists2  :: forall f x y. f x y -> Some2 f

-- data CFSome :: (k -> Constraint) -> (k -> *) -> * where
--   ExistsCF  :: forall c f x. c f => f x   -> CFSome c f
