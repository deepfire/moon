module Data.TypeRep
  ( showSomeTypeRep
  , showTypeRep
  )
where

import Data.Text
import Type.Reflection


showSomeTypeRep :: SomeTypeRep -> Text
showSomeTypeRep (SomeTypeRep r)  = showTypeRep r

showTypeRep :: TypeRep (a :: k) -> Text
showTypeRep (App f x)  = "(App "<>showTypeRep f<>" "<>showTypeRep x<>")"
showTypeRep (Con name) = "(Con "<>pack (show name)<>")"
showTypeRep (Fun a b)  = "(Fun "<>showTypeRep a<>" -> "<>showTypeRep b<>")"
