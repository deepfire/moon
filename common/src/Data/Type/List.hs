module Data.Type.List
  ( Append
  , Reverse
  )
where

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
