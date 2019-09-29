{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-|
Module      : Data.RTTI
Description : Reflection for GADTs
Copyright   : (c) Kosyrev Serge, 2019
License     : GPL-3

Supporting types for "Binary instances for GADTs (or: RTTI in Haskell)",
by Edsko de Vries: https://www.well-typed.com/blog/2017/06/rtti/
-}
module Data.RTTI
  ( RTTI
  , RTTI2)
where


-- https://www.well-typed.com/blog/2017/06/rtti/

data family RTTI  (f :: k -> *)       :: (k -> *)

data family RTTI2 (f :: k1 -> * -> *) :: (k1 -> * -> *)
