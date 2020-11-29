{-# LANGUAGE UndecidableInstances       #-}
module Dom.Space (module Dom.Space) where

import qualified Algebra.Graph.AdjacencyMap       as GA
import           Data.Coerce                        (coerce)
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Sequence                    as Seq
import qualified Data.Set                         as Set'
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

import Basis

import Dom.CTag
import Dom.Name
import Dom.Scope


--------------------------------------------------------------------------------
-- * Space:  a tree of 'Scope's of Name'd things
--
data Space c a = Space
  { nsTree :: !(GA.AdjacencyMap (QName Scope))
  , nsMap  :: !(MonoidalMap     (QName Scope) (Scope c a))
  } deriving Generic

type PointSpace a = Space 'Point a

--------------------------------------------------------------------------------
-- * Instances
--
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

--------------------------------------------------------------------------------
-- * Utils
--
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
         $ trace (unpack $ mconcat
                  [ "alterSpace: scope=", showQName scName
                  , ", name=", showName name
                  , ", fqname=", showQName fqname
                  ])
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
