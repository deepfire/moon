{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module SomeValue
  ( SomeValueKinded(..)
  , SomeValue(..)
  , readSomeValue
  , stripValue
  , stripSomeValue
  , unitSomeValue
  , someValueSomeCTag
  , someValueSomeTypeRep
  , someValueKindedSomeTypeRep
  , someValueSomeType
  , withSomeValue
  )
where

import qualified Data.Set.Monad                   as Set
import           Text.Read                           (ReadPrec)
import qualified Type.Reflection                  as R

import Basis

import Type
import SomeType

--------------------------------------------------------------------------------
-- * Ground
--
data SomeValueKinded (c :: Con) =
  forall a. Ground a =>
  SomeValueKinded (Value c a)

data SomeKindValue a =
  forall (c :: Con). Typeable c =>
  SomeKindValue (CTag c) (Value c a)

data SomeValue =
  forall c. (ReifyCTag c, Typeable c) =>
  SomeValue (CTag c) (SomeValueKinded c)
-- data SomeValue = forall a. Ground a =>
--   SomeValue  (SomeKindValue a)

readSomeValue :: forall (c :: Con). CTag c -> TyDict Ground -> ReadPrec SomeValue
readSomeValue tag (TyDict (a :: Proxy a)) =
  case tag of
    TPoint -> do
      v :: a <- readPrec
      pure . SomeValue tag . SomeValueKinded $ mkValue' a tag v
    TList -> do
      v :: [a] <- readListPrec
      pure . SomeValue tag . SomeValueKinded $ mkValue' a tag v
    TSet -> do
      v :: [a] <- readListPrec
      pure . SomeValue tag . SomeValueKinded $ mkValue' a tag $ Set.fromList v
    _ -> trace (printf "No parser for structures outside of Point/List/Set.")
               (fail "")

unitSomeValue :: SomeValue
unitSomeValue = SomeValue TPoint $ SomeValueKinded (VPoint ())

stripValue ::
  forall (c :: Con) a
  .  Value c a
  -> Repr c a
stripValue = \case
  VPoint x -> x
  VList  x -> x
  VSet   x -> x
  VTree  x -> x
  VDag   x -> x
  VGraph x -> x

stripSomeValue ::
  forall a c
   . (Typeable a, Typeable c)
  => CTag c
  -> Proxy a
  -> SomeValue
  -> Maybe (Repr c a)
stripSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded (r :: Value c' a'))) =
  let exptr = typeRep @a
      svtr  = typeRep @a'
      expk  = typeRep @c
      svk   = typeRep @c'
  in case (,) (svtr `R.eqTypeRep` exptr)
              (svk  `R.eqTypeRep` expk) of
    (Just R.HRefl, Just R.HRefl) -> Just $ stripValue r
    _ -> Nothing

withSomeValue
  :: forall a c b
   . (Typeable a, Typeable c)
  => CTag c
  -> Proxy a
  -> SomeValue
  -> (Value c a -> b)
  -> Either Text b
withSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded (r :: Value c' a'))) f =
  let exptr = typeRep @a
      svtr  = typeRep @a'
      expk  = typeRep @c
      svk   = typeRep @c'
  in case (,) (svtr `R.eqTypeRep` exptr)
              (svk  `R.eqTypeRep` expk) of
    (Just R.HRefl, Just R.HRefl) -> Right $ f r
    _ -> Left . pack $ printf "withSomeValue: expected %s/%s, got %s/%s"
                (show exptr) (show expk) (show svtr) (show svk)

someValueSomeCTag :: SomeValue -> SomeCTag
someValueSomeCTag (SomeValue t _) = SomeCTag t

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue _ svk) = someValueKindedSomeTypeRep svk

someValueKindedSomeTypeRep :: SomeValueKinded c -> R.SomeTypeRep
someValueKindedSomeTypeRep (SomeValueKinded (_ :: Value c a)) = R.someTypeRep $ Proxy @a

someValueSomeType :: SomeValue -> SomeType
someValueSomeType (SomeValue ctag (SomeValueKinded (_ :: Value c a))) =
  ctagSomeType ctag (Proxy @a)

instance Show SomeValue where
  show (SomeValue _ (SomeValueKinded x)) = show x
