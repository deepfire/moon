module Data.Class (module Data.Class) where

import Data.Proxy
import Unsafe.Coerce                    qualified as Unsafe



-- * Reflection
--
newtype Magic c r = Magic (forall a. c a => Proxy a -> r)
reifyConstr :: forall r b c. Proxy c -> b -> (forall a. c a => Proxy a -> r) -> r
reifyConstr _p n k = Unsafe.unsafeCoerce
                       (Magic k :: Magic c r)
                       n
                       Proxy
{-# INLINE reifyConstr #-}
