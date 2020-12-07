module Data.TypeRep (module Data.TypeRep) where

import Data.Text
import Text.Builder
import Type.Reflection


showSomeTypeRep :: SomeTypeRep -> Text
showSomeTypeRep (SomeTypeRep r)  = showTypeRep r

showTypeRep :: TypeRep (a :: k) -> Text
showTypeRep (App f x)  = "(App "<>showTypeRep f<>" "<>showTypeRep x<>")"
showTypeRep (Con name) = "(Con "<>pack (show name)<>")"
showTypeRep (Fun a b)  = "(Fun "<>showTypeRep a<>" -> "<>showTypeRep b<>")"

listTyCon, tuple2TyCon, tuple3TyCon, charTyCon :: TyCon
listTyCon   = typeRepTyCon $ typeRep @[()]
tuple2TyCon = typeRepTyCon $ typeRep @((),())
tuple3TyCon = typeRepTyCon $ typeRep @((),(),())
charTyCon   = typeRepTyCon $ typeRep @Char

showSomeTypeRepNoKind :: SomeTypeRep -> Text
showSomeTypeRepNoKind (SomeTypeRep x) = showTypeRepNoKind x

showTypeRepNoKind :: TypeRep a -> Text
showTypeRepNoKind = run . flip go False
 where
   go :: TypeRep b -> Bool -> Builder
   go (App (Con f) a1) _
     | f == listTyCon =
       case a1 of
         Con x | x == charTyCon
           -> text "String"
         _ -> char '[' <> go a1 False <> char ']'
   go (App (App (Con f) a1) a2) _
     | f == tuple2TyCon =
       char '(' <> go a1 False <> char ',' <> char ' ' <> go a2 False <> char ')'
   go (App (App (App (Con f) a1) a2) a3) _
     | f == tuple3TyCon =
       char '(' <> go a1 False <> char ',' <> char ' ' <> go a2 False <> char ',' <> char ' ' <> go a3 False <> char ')'
   go (Con c) _ =
     string $ show c
   go a@App{} True =
     char '(' <> go a False <> char ')'
   go (App f x) False =
     go f True <> char ' ' <> go x True
   go f@Fun{} True =
     char '(' <> go f False <> char ')'
   go (Fun x r) False =
     go x True <> text " -> " <> go r True
