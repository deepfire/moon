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
import Dom.CTag
import Dom.Name


data Space c a = Space
  { nsTree :: !(GA.AdjacencyMap (QName Scope))
  , nsMap  :: !(MonoidalMap     (QName Scope) (Scope c a))
  } deriving Generic

type PointSpace a = Space 'Point a

instance ReifyCTag c => Functor (Space c) where
  fmap f s@Space{nsMap} = s { nsMap = (f <$>) <$> nsMap }

deriving instance Eq (Repr c a) => Eq (Space c a)
deriving instance Ord (Repr c a) => Ord (Space c a)

instance Semigroup (Space c a) where
  Space{nsTree=lt, nsMap=lm} <> Space{nsTree=rt, nsMap=rm}
    = Space (lt `GA.overlay` rt) (lm <> rm)

instance Monoid (Space c a) where
  mempty = emptySpace

instance Serialise (Repr c a) => Serialise (Space c a)

-- XXX: inefficient
spaceQNameRMap :: (Eq (Repr c a), Hashable (Repr c a))
               => Space c a -> HashMap (Repr c a) (QName a)
spaceQNameRMap Space{..} =
  HashMap.fromList . concat $
  MMap.toList nsMap
  <&> \(scName, Scope{..})->
        Map.toList sMap
          <&> swap . first (coerceQName . append scName . coerceName)

emptySpace :: Space c a
emptySpace = Space
  { nsTree = GA.vertex mempty
  , nsMap  = mempty
  }

insertScope :: QName Scope -> Scope c a -> Space c a -> Space c a
insertScope prefix sc ns = insertScopeAt fqname sc ns
 where
   fqname = prefix `append` coerceName (sName sc)

insertScopeAt :: QName Scope -> Scope c a -> Space c a -> Space c a
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
  :: forall c a
  . QName Scope
  -> [Scope c a]
  -> Space c a
  -> Space c a
attachScopes sub scopes ns =
  foldr (insertScope sub) ns scopes

scopeAt :: QName Scope -> Space c a -> Maybe (Scope c a)
scopeAt q ns = MMap.lookup q (nsMap ns)

updateSpaceScope
  :: (e ~ Text, Typeable a)
  => (Scope c a -> Scope c a)
  -> QName Scope
  -> Space c a
  -> Space c a
updateSpaceScope f name ns =
  ns { nsMap  = MMap.update (Just . f) name (nsMap ns) }

alterSpaceScope
  :: (e ~ Text, Typeable a)
  => (Maybe (Scope c a) -> Either e (Maybe (Scope c a)))
  -> QName Scope
  -> Space c a
  -> Either e (Space c a)
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
  :: forall c a (f :: Con -> * -> *) e. (e ~ Text)
  => Text
  -> QName         a
  -> Either e (f c a)
  -> (Maybe   (f c a) -> Either e (Maybe (f c a)))
_failHasEntity ty name _ (Just _) = Left $ "Already has "<>ty<>": " <> showQName name
_failHasEntity _  _    x  Nothing = Just <$> x

childScopeQNamesAt :: QName Scope -> Space c a -> [QName Scope]
childScopeQNamesAt q ns =
  Set'.toList (GA.postSet q (nsTree ns))

_checkBusy :: QName Scope -> Space c a -> Bool
_checkBusy name ns = MMap.member name (nsMap ns)

lookupSpace :: QName a -> Space c a -> Maybe (Repr c a)
lookupSpace n s = withQName n $
  \scName -> \case
    Nothing   -> Nothing
    Just name -> scopeAt scName s >>= lookupScope name

lookupSpaceScope :: QName Scope -> Space c a -> Maybe (Scope c a)
lookupSpaceScope n s = MMap.lookup n (nsMap s)

spaceEntries :: Space c a -> [Repr c a]
spaceEntries ns = concatMap scopeEntries $ MMap.elems (nsMap ns)

spaceScopes :: Space c a -> [(QName Scope, Scope c a)]
spaceScopes ns = MMap.toList (nsMap ns)

alterSpace
  :: (e ~ Text, Typeable a)
  => QName a
  -> Space c a
  -> (Maybe (Repr c a) -> Either e (Maybe (Repr c a)))
  -> Either e (Space c a)
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
  :: forall c a e. (e ~ Text, Typeable a)
  => QName a
  -> Repr c a
  -> Space c a -> Either e (Space c a)
spaceAdd name x ns =
  alterSpace name ns
  (\case
      Nothing -> Right $ Just x
      Just  _ -> Left $ "Already has element: " <> showQName name)

spaceUpdate
  :: forall c a e. (e ~ Text, Typeable a)
  => QName a
  -> (Repr c a -> Repr c a)
  -> Space c a -> Either e (Space c a)
spaceUpdate name f ns =
  alterSpace name ns
  (\case
      Nothing -> Left $ "Already has element: " <> showQName name
      Just  x -> Right . Just . f $ x)


data Scope c a = Scope
  { sName :: !(Name Scope)
  , sMap  :: !(Map (Name a) (Repr c a))
  } deriving Generic
deriving instance Eq (Repr c a) => Eq (Scope c a)
deriving instance Ord (Repr c a) => Ord (Scope c a)
-- (Eq, Generic, Ord, Show)

type PointScope a = Scope 'Point a

instance ReifyCTag c => Functor (Scope c) where
  fmap f s@Scope{sMap} =
    s { sMap  = Unsafe.unsafeCoerce $ mapRepr (reifyCTag $ Proxy @c) f <$> sMap }

instance Serialise (Repr c a) => Serialise (Scope c a)

scopeName :: Scope c a -> Name Scope
scopeName = sName

instance Semigroup (Scope c a) where
  Scope{sName, sMap=l} <> Scope{sMap=r}
    = Scope sName (l <> r)

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
