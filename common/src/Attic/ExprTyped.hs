module Attic.ExprTyped
  ( Expr'(..)
  , Apply
  , Compose)
where

import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty

-- * Experimental
--
data Expr' k where
  V  ::  Expr' r
  Ap :: r ~ Apply f x
    =>
    { xapF :: Expr' f
    , xapX :: Expr' x
    } -> Expr' r
  Co ::r ~ Compose f g
    =>
    { xcoF :: Expr' f
    , xcoG :: Expr' g
    } -> Expr' r

_v = Ap (V @(String -> ()) `Co` V @(Int -> String)) (V @Int)

type family Apply f x where
  Apply (x -> y) x = y
  Apply (x -> y) z = TypeError
    (Ty.Text "Applying function type ("
     :<>: ShowType x :<>: Ty.Text " -> " :<>: ShowType y
     :<>: Ty.Text ") to incompatible type " :<>: ShowType z)
  Apply f x = TypeError
    (Ty.Text "Applying a non-function type " :<>: ShowType f
     :<>: Ty.Text " to type " :<>: ShowType x)

type family Compose f g where
  Compose (y -> z) (x -> y) = x -> z
  Compose f g = TypeError
    (Ty.Text "Applying non-composable types "
     :<>: ShowType f :<>: Ty.Text " and " :<>: ShowType g)
