{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module SomeValue
  ( SomeValue(..)
  , SomeKindValue(..)
  , someValueSomeTypeRep
  , someValueSomeType
  , withSomeValue
  )
where

import qualified Type.Reflection                  as R

import Basis

import Type
import SomeType

--------------------------------------------------------------------------------
-- * Ground
--
data SomeKindValue a =
  forall (k :: Con). Typeable k
  => SomeKindValue (Tag k) (Value k a)

data SomeValue = forall a. Ground a => SomeValue  (SomeKindValue a)

withSomeValue
  :: forall a k b
   . (Ground a, Typeable k)
  => Tag k
  -> Proxy a
  -> SomeValue
  -> (Value k a -> b)
  -> Either Text b
withSomeValue _ _ (SomeValue (SomeKindValue (_ :: Tag k') r :: SomeKindValue a')) f =
  let exptr = typeRep @a
      svtr  = typeRep @a'
      expk  = typeRep @k
      svk   = typeRep @k'
  in case (,) (svtr `R.eqTypeRep` exptr)
              (svk  `R.eqTypeRep` expk) of
    (Just R.HRefl, Just R.HRefl) -> Right $ f r
    _ -> Left . pack $ printf "withSomeValue: expected %s/%s, got %s/%s"
                (show exptr) (show expk) (show svtr) (show svk)

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue (_ :: SomeKindValue a)) = R.someTypeRep $ Proxy @a

someValueSomeType :: SomeValue -> SomeType
someValueSomeType (SomeValue (SomeKindValue tag (_ :: Value k a))) =
  tagSomeType tag (Proxy @a)


instance (Show a) => Show (SomeKindValue a) where
  show (SomeKindValue _ x) = "(SKV "<>show x<>")"

instance Show SomeValue where
  show (SomeValue (SomeKindValue _ x)) = show x
