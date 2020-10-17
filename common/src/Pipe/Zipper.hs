module Pipe.Zipper
  ( ZExpr
  , fromExpr
  , rezip
  , left
  , right
  , up
  , top
  , upFromLeft'
  , upFromRight'
  )
where

import Ground.Expr


type ZExpr p = ([Either (Expr p) (Expr p)], Expr p)

fromExpr :: Expr p -> ZExpr p
fromExpr = (,) []

left :: ZExpr p -> Maybe (ZExpr p)
left (xs, x) = case x of
  PVal{}    -> Nothing
  PPipe{}   -> Nothing
  PApp  l _ -> Just (Right x:xs, l)
  PComp l _ -> Just (Right x:xs, l)

right :: ZExpr p -> Maybe (ZExpr p)
right (xs, x) = case x of
  PVal{}    -> Nothing
  PPipe{}   -> Nothing
  PApp  _ r -> Just (Left  x:xs, r)
  PComp _ r -> Just (Left  x:xs, r)

upFromLeft'  :: ZExpr p -> ZExpr p
upFromLeft'  (Left  (PApp  l _):xs, x) = (xs, PApp  l x)
upFromLeft'  (Left  (PComp l _):xs, x) = (xs, PComp l x)
upFromLeft' _ = error "upFromLeft'"

upFromRight' :: ZExpr p -> ZExpr p
upFromRight' (Right (PApp  _ r):xs, x) = (xs, PApp  x r)
upFromRight' (Right (PComp _ r):xs, x) = (xs, PComp x r)
upFromRight' _ = error "upFromRight'"

up :: ZExpr p -> Maybe (ZExpr p)
up ([], _)         = Nothing
up z@(Left _:_, _) = Just $ upFromLeft' z
up _ = error "up"

top :: ZExpr p -> ZExpr p
top z@(Left  _:_, _) = upFromLeft'  z
top z@(Right _:_, _) = upFromRight' z
top z@([], _)        = z

rezip :: ZExpr p -> Expr p
rezip = snd . top
