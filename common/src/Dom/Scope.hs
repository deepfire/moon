{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Dom.Scope (module Dom.Scope) where

import           Data.Coerce                        (coerce)
import           Data.Map.Monoidal.Strict           (MonoidalMap(..))
import qualified Data.Map.Strict                  as Map
import qualified Data.Sequence                    as Seq
import           GHC.Generics                       (Generic)

import qualified Unsafe.Coerce                    as Unsafe -- Papering over Names

import Basis

import Dom.CTag
import Dom.Name


--------------------------------------------------------------------------------
-- * Scope of Name'd things
--
data Scope c a = Scope
  { sName :: !(Name Scope)
  , sMap  :: !(Map (Name a) (Repr c a))
  }
  deriving Generic

type PointScope a = Scope 'Point a

--------------------------------------------------------------------------------
-- * Instances
--
deriving instance Eq (Repr c a) => Eq (Scope c a)
deriving instance Ord (Repr c a) => Ord (Scope c a)

instance ReifyCTag c => Functor (Scope c) where
  fmap f s@Scope{sMap} =
    s { sMap = Unsafe.unsafeCoerce $ mapRepr @c f <$> sMap }

-- Caveat:  non-law-abiding.
instance ReifyCTag c => Applicative (Scope c) where
  pure :: forall a. ReifyCTag c => a -> Scope c a
  pure x = Scope "" $ Map.singleton "" (pureRepr @c x)
  (<*>) :: forall a b. ReifyCTag c => Scope c (a -> b) -> Scope c a -> Scope c b
  Scope _ mf <*> Scope n mv =
    let fn   = pickRepr @c (snd . head $ Map.toList mf) :: a -> b
    in Scope n (Unsafe.unsafeCoerce mv -- Name coercions are safe, within reason.
                & fmap (mapRepr @c fn))

instance ReifyCTag c => Foldable (Scope c) where
  foldMap toM Scope{..} = mconcat $ foldMapRepr @c toM <$> Map.elems sMap

instance ReifyCTag c => Traversable (Scope c) where
  traverse f s@Scope{..} =
    traverse (traverseRepr @c f) sMap <&> \m -> s { sMap = Unsafe.unsafeCoerce m }

instance Serialise (Repr c a) => Serialise (Scope c a)

instance Semigroup (Scope c a) where
  Scope{sName, sMap=l} <> Scope{sMap=r}
    = Scope sName (l <> r)

--------------------------------------------------------------------------------
-- * Utils
--
scopeName :: Scope c a -> Name Scope
scopeName = sName

emptyScope :: Name Scope -> Scope c a
emptyScope = flip Scope mempty

scope :: Name Scope -> [(Name a, Repr c a)] -> Scope c a
scope name = Scope name . Map.fromList

scopeSize :: Scope c a -> Int
scopeSize s = Map.size (sMap s)

lookupScope :: Name a -> Scope c a -> Maybe (Repr c a)
lookupScope n s = Map.lookup n (sMap s)

selectFromScope :: (Name a -> Repr c a -> Bool) -> Scope c a -> [Repr c a]
selectFromScope f s =
  [ v
  | (k, v) <- Map.toList (sMap s)
  , f k v ]

scopeNames :: Scope c a -> Set (Name a)
scopeNames = keysSet . sMap

scopeEntries :: Scope c a -> [Repr c a]
scopeEntries = Map.elems . sMap

mapScope :: (Repr c a -> Repr c a) -> Scope c a -> Scope c a
mapScope f Scope{..} =
  Scope sName $ f <$> sMap

-- | Interpret a QName into its parent scope and name components.
withQName :: QName a -> (QName Scope -> Maybe (Name a) -> b) -> b
withQName (QName ns) f
  | Seq.Empty           <- ns = f mempty     Nothing
  | Seq.Empty Seq.:|> x <- ns = f mempty     (Just x)
  | xs Seq.:|> x        <- ns = f (QName $ coerceName <$> xs) (Just x)

updateScope
  :: Typeable a
  => (Repr c a -> Maybe (Repr c a))
  -> Name a
  -> Scope c a -> Scope c a
updateScope f name s =
  s { sMap = Map.update f name (sMap s) }

alterScope
  :: (e ~ Text, Typeable a)
  => (Maybe (Repr c a) -> Either e (Maybe (Repr c a)))
  -> Name a
  -> Scope c a -> Either e (Scope c a)
alterScope f name s =
  (\m -> s { sMap = m }) <$> Map.alterF f name (sMap s)

alterFMonoidalMap    :: forall f c a e. (Functor f, Ord c, f ~ Either e) =>
                        (Maybe a -> f (Maybe a)) -> c -> MonoidalMap c a -> f (MonoidalMap c a)
alterFMonoidalMap =
  coerce (Map.alterF :: (Maybe a -> f (Maybe a)) -> c -> Map         c a -> f (Map         c a))
{-# INLINE alterFMonoidalMap #-}
