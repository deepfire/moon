{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Namespace
  ( Space(..)
  , PointSpace
  , emptySpace
  , insertScope
  , insertScopeAt
  , attachScopes
  , spaceQNameRMap
  , scopeAt
  , childScopeQNamesAt
  , updateSpaceScope
  , lookupSpace
  , lookupSpaceScope
  , spaceEntries
  , spaceScopes
  , alterSpace
  , spaceAdd
  , spaceUpdate
  , Scope
  , PointScope
  , scopeName
  , emptyScope
  , scope
  , scopeSize
  , lookupScope
  , selectFromScope
  , scopeNames
  , scopeEntries
  , mapScope
  , withQName
  , updateScope
  , alterScope
  -- * Re-exports
  , module Type
  )
where

import qualified Algebra.Graph.AdjacencyMap       as GA
import           Codec.Serialise
import           Data.Coerce                        (coerce)
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set                         as Set'
import qualified Data.Sequence                    as Seq
import           GHC.Generics                       (Generic)

import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Type


data Space k a = Space
  { nsTree :: !(GA.AdjacencyMap (QName Scope))
  , nsMap  :: !(MonoidalMap     (QName Scope) (Scope k a))
  } deriving Generic

type PointSpace a = Space 'Point a

instance ReifyTag k => Functor (Space k) where
  fmap f s@Space{nsMap} = s { nsMap = (f <$>) <$> nsMap }

deriving instance Eq (Repr k a) => Eq (Space k a)
deriving instance Ord (Repr k a) => Ord (Space k a)

instance Semigroup (Space k a) where
  Space{nsTree=lt, nsMap=lm} <> Space{nsTree=rt, nsMap=rm}
    = Space (lt `GA.overlay` rt) (lm <> rm)

instance Monoid (Space k a) where
  mempty = emptySpace

instance Serialise (Repr k a) => Serialise (Space k a)

-- XXX: inefficient
spaceQNameRMap :: (Eq (Repr k a), Hashable (Repr k a))
               => Space k a -> HashMap (Repr k a) (QName a)
spaceQNameRMap Space{..} =
  HashMap.fromList . concat $
  MMap.toList nsMap
  <&> \(scName, Scope{..})->
        Map.toList sMap
          <&> swap . first (coerceQName . append scName . coerceName)

emptySpace :: Space k a
emptySpace = Space
  { nsTree = GA.vertex mempty
  , nsMap  = mempty
  }

insertScope :: QName Scope -> Scope k a -> Space k a -> Space k a
insertScope prefix sc ns = insertScopeAt fqname sc ns
 where
   fqname = prefix `append` coerceName (sName sc)

insertScopeAt :: QName Scope -> Scope k a -> Space k a -> Space k a
insertScopeAt name sc ns =
  ns { nsMap  = MMap.insert (coerceQName name) sc $ nsMap ns
     , nsTree = nsTree ns
                `GA.overlay`
                withQName name
                (curry $ \case
                    (_, Nothing)
                      -> GA.vertex name
                    (QName (Seq.null -> True), _)
                      -> GA.edge mempty name
                    (parent, _)
                      -> GA.edge parent name)
     }

attachScopes
  :: forall k a
  . QName Scope
  -> [Scope k a]
  -> Space k a
  -> Space k a
attachScopes sub scopes ns =
  foldr (insertScope sub) ns scopes

scopeAt :: QName Scope -> Space k a -> Maybe (Scope k a)
scopeAt q ns = MMap.lookup q (nsMap ns)

updateSpaceScope
  :: (e ~ Text, Typeable a)
  => (Scope k a -> Scope k a)
  -> QName Scope
  -> Space k a
  -> Space k a
updateSpaceScope f name ns =
  ns { nsMap  = MMap.update (Just . f) name (nsMap ns) }

alterSpaceScope
  :: (e ~ Text, Typeable a)
  => (Maybe (Scope k a) -> Either e (Maybe (Scope k a)))
  -> QName Scope
  -> Space k a
  -> Either e (Space k a)
alterSpaceScope f name ns =
  (\m -> ns { nsMap = m }) <$> alterFMonoidalMap f name (nsMap ns)

failNoEntity
  :: (e ~ Text)
  => Text
  -> QName  a
  ->       (a -> Either e (Maybe a))
  -> (Maybe a -> Either e (Maybe a))
failNoEntity ty name _  Nothing = Left $ "No such "<>ty<>": " <> showQName name
failNoEntity _  _    f (Just x) = f x

_failHasEntity
  :: forall k a (f :: Con -> * -> *) e. (e ~ Text)
  => Text
  -> QName         a
  -> Either e (f k a)
  -> (Maybe   (f k a) -> Either e (Maybe (f k a)))
_failHasEntity ty name _ (Just _) = Left $ "Already has "<>ty<>": " <> showQName name
_failHasEntity _  _    x  Nothing = Just <$> x

childScopeQNamesAt :: QName Scope -> Space k a -> [QName Scope]
childScopeQNamesAt q ns =
  Set'.toList (GA.postSet q (nsTree ns))

_checkBusy :: QName Scope -> Space k a -> Bool
_checkBusy name ns = MMap.member name (nsMap ns)

lookupSpace :: QName a -> Space k a -> Maybe (Repr k a)
lookupSpace n s = withQName n $
  \scName -> \case
    Nothing   -> Nothing
    Just name -> scopeAt scName s >>= lookupScope name

lookupSpaceScope :: QName Scope -> Space k a -> Maybe (Scope k a)
lookupSpaceScope n s = MMap.lookup n (nsMap s)

spaceEntries :: Space k a -> [Repr k a]
spaceEntries ns = concatMap scopeEntries $ MMap.elems (nsMap ns)

spaceScopes :: Space k a -> [(QName Scope, Scope k a)]
spaceScopes ns = MMap.toList (nsMap ns)

alterSpace
  :: (e ~ Text, Typeable a)
  => QName a
  -> Space k a
  -> (Maybe (Repr k a) -> Either e (Maybe (Repr k a)))
  -> Either e (Space k a)
alterSpace fqname ns f =
  withQName fqname $
    \scName -> \case
      Nothing -> Left $ "Malformed QName: " <> showQName fqname
      Just name ->
        alterSpaceScope
        (failNoEntity "scope" (coerceQName scName)
         $ (sequence . Just <$>)
         $ alterScope f name)
        scName ns

spaceAdd
  :: forall k a e. (e ~ Text, Typeable a)
  => QName a
  -> Repr k a
  -> Space k a -> Either e (Space k a)
spaceAdd name x ns =
  alterSpace name ns
  (\case
      Nothing -> Right $ Just x
      Just  _ -> Left $ "Already has element: " <> showQName name)

spaceUpdate
  :: forall k a e. (e ~ Text, Typeable a)
  => QName a
  -> (Repr k a -> Repr k a)
  -> Space k a -> Either e (Space k a)
spaceUpdate name f ns =
  alterSpace name ns
  (\case
      Nothing -> Left $ "Already has element: " <> showQName name
      Just  x -> Right . Just . f $ x)


data Scope k a = Scope
  { sName :: !(Name Scope)
  , sMap  :: !(Map (Name a) (Repr k a))
  } deriving Generic
deriving instance Eq (Repr k a) => Eq (Scope k a)
deriving instance Ord (Repr k a) => Ord (Scope k a)
-- (Eq, Generic, Ord, Show)

type PointScope a = Scope 'Point a

instance ReifyTag k => Functor (Scope k) where
  fmap f s@Scope{sMap} =
    s { sMap  = Unsafe.unsafeCoerce $ mapRepr (reifyTag $ Proxy @k) f <$> sMap }

instance Serialise (Repr k a) => Serialise (Scope k a)

scopeName :: Scope k a -> Name Scope
scopeName = sName

instance Semigroup (Scope k a) where
  Scope{sName, sMap=l} <> Scope{sMap=r}
    = Scope sName (l <> r)

emptyScope :: Name Scope -> Scope k a
emptyScope = flip Scope mempty

scope :: Name Scope -> [(Name a, Repr k a)] -> Scope k a
scope name = Scope name . Map.fromList

scopeSize :: Scope k a -> Int
scopeSize s = Map.size (sMap s)

lookupScope :: Name a -> Scope k a -> Maybe (Repr k a)
lookupScope n s = Map.lookup n (sMap s)

selectFromScope :: (Name a -> Repr k a -> Bool) -> Scope k a -> [Repr k a]
selectFromScope f s =
  [ v
  | (k, v) <- Map.toList (sMap s)
  , f k v ]

scopeNames :: Scope k a -> Set (Name a)
scopeNames = keysSet . sMap

scopeEntries :: Scope k a -> [Repr k a]
scopeEntries = Map.elems . sMap

mapScope :: (Repr k a -> Repr k a) -> Scope k a -> Scope k a
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
  => (Repr k a -> Maybe (Repr k a))
  -> Name a
  -> Scope k a -> Scope k a
updateScope f name s =
  s { sMap = Map.update f name (sMap s) }

alterScope
  :: (e ~ Text, Typeable a)
  => (Maybe (Repr k a) -> Either e (Maybe (Repr k a)))
  -> Name a
  -> Scope k a -> Either e (Scope k a)
alterScope f name s =
  (\m -> s { sMap = m }) <$> Map.alterF f name (sMap s)

alterFMonoidalMap    :: forall f k a e. (Functor f, Ord k, f ~ Either e) =>
                        (Maybe a -> f (Maybe a)) -> k -> MonoidalMap k a -> f (MonoidalMap k a)
alterFMonoidalMap =
  coerce (Map.alterF :: (Maybe a -> f (Maybe a)) -> k -> Map         k a -> f (Map         k a))
{-# INLINE alterFMonoidalMap #-}
