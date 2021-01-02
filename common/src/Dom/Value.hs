{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Dom.Value (module Dom.Value) where

import           Data.Typeable                      (Proxy, Typeable)

import Dom.CTag
import Dom.VTag


--------------------------------------------------------------------------------
-- * Concrete values
--
data Value (c :: Con) a where
  VPoint  :: Repr Point a -> Value Point a
  VList   :: Repr List  a -> Value List  a
  VSet    :: Repr 'Set  a -> Value 'Set  a
  VTree   :: Repr Tree  a -> Value Tree  a
  VDag    :: Repr Dag   a -> Value Dag   a
  VGraph  :: Repr Graph a -> Value Graph a
  deriving (Typeable)

instance (Show a) => Show (Value c a) where
  show (VPoint x) = "VPoint " <> show x
  show (VList  x) = "VList "  <> show x
  show (VSet   x) = "VSet "   <> show (foldMap (:[]) x)
  show (VTree  x) = "VTree "  <> show x
  show (VDag   x) = "VDag "   <> show x
  show (VGraph x) = "VGraph " <> show x

instance Functor (Value c) where
  fmap f (VPoint x) = VPoint $ f     x
  fmap f (VList x)  = VList  $ f <$> x
  fmap f (VSet x)   = VSet   $ f <$> x
  fmap f (VTree x)  = VTree  $ f <$> x
  fmap f (VDag x)   = VDag   $ f <$> x
  fmap f (VGraph x) = VGraph $ f <$> x

mkValue :: CTag k -> VTag a -> Repr k a -> Value k a
mkValue = \case
  CPoint -> const VPoint
  CList  -> const VList
  CSet   -> const VSet
  CTree  -> const VTree
  CDag   -> const VDag
  CGraph -> const VGraph

mkValue' :: Proxy a -> CTag c -> Repr c a -> Value c a
mkValue' = const $ \case
  CPoint -> VPoint
  CList  -> VList
  CSet   -> VSet
  CTree  -> VTree
  CDag   -> VDag
  CGraph -> VGraph

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
