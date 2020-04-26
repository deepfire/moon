module Data.Type.List
  ( Append
  , Dict(..)
  , Reverse
  , spineConstraint
  )
where

import Data.SOP (All(..))
import Data.SOP.Dict (Dict(..))
import Data.Typeable

-- Stolen from: type-list-0.5.0.0
-- |Helper type family for 'Reverse'.
type family ReverseAcc xs acc where
    ReverseAcc '[] acc = acc
    ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

-- Stolen from: type-list-0.5.0.0
-- |Reverse a type-level list.
type family Reverse xs where
    Reverse xs = ReverseAcc xs '[]

type family Append x xs where
    Append x xs = Reverse (x : Reverse xs)

spineConstraint :: forall k (xs :: [k]) . (Typeable k, All Typeable xs) => Dict Typeable xs
spineConstraint = cpara_SList (Proxy @Typeable) Dict (\ Dict -> Dict)
