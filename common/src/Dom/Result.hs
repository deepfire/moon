{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Dom.Result (module Dom.Result) where

import Reflex

import Dom.Error
import Dom.LTag
import Dom.SomeValue


-- | Result of running a pipe.
type family Result l a where
  Result  Now       a = IO (Fallible a)
  Result (Live t m) a = m (Event t (Fallible a))

data SomeResult
  = forall l. (LiveConstr l) =>
    SR
    { srLTag  :: LTag l
    , srValue :: Result l SomeValue
    }
