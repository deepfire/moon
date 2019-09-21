{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Data.RTTI
  (RTTI(..), RTTI2(..))
where

import Data.Kind (Constraint, Type)


data family RTTI  (f :: k -> *)       :: (k -> *)

data family RTTI2 (f :: k1 -> * -> *) :: (k1 -> * -> *)
