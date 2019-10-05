{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
module Namespace
  ( Space
  , emptySpace
  , insertScope
  , attachScopes
  , scopeAt
  , childScopeNamesAt
  , updateSpaceScope
  , lookupSpace
  , spaceEntries
  , spaceScopes
  , alterSpace
  , spaceAdd
  , spaceUpdate
  , Scope
  , scopeName
  , emptyScope
  , scope
  , lookupScope
  , scopeNames
  , scopeEntries
  , withQScopeName
  , updateScope
  , alterScope
  -- * Re-exports
  , module Type
  )
where

import qualified Algebra.Graph.AdjacencyMap       as GA
import           Data.Coerce                        (coerce)
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set                         as Set'
import qualified Data.Sequence                    as Seq
import           GHC.Generics                       (Generic)

import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Type


data Space k a = Space
  { nsTree :: !(GA.AdjacencyMap (QName (Scope k a)))
  , nsMap  :: !(MonoidalMap     (QName (Scope k a)) (Scope k a))
  }

instance Semigroup (Space k a) where
  Space{nsTree=lt, nsMap=lm} <> Space{nsTree=rt, nsMap=rm}
    = Space (lt `GA.overlay` rt) (lm <> rm)

instance Monoid (Space k a) where
  mempty = emptySpace

emptySpace :: Space k a
emptySpace = Space
  { nsTree = GA.vertex mempty
  , nsMap  = MMap.fromList
    [ -- TODO: Try remove this null entry
      (mempty, emptyScope "")
    ]
  } where

insertScope :: QName (Scope k a) -> Scope k a -> Space k a -> Space k a
insertScope prefix scope ns =
  ns { nsMap  = MMap.insert name scope $ nsMap ns
     , nsTree = nsTree ns
                `GA.overlay`
                GA.edge prefix name
     } where
  name  = prefix `append` sName scope

attachScopes
  :: forall k a
  . QName (Scope k a)
  -> [Scope k a]
  -> Space k a
  -> Space k a
attachScopes sub scopes ns =
  foldr (insertScope sub) ns scopes

scopeAt :: QName (Scope k a) -> Space k a -> Maybe (Scope k a)
scopeAt q ns = MMap.lookup q (nsMap ns)

updateSpaceScope
  :: (e ~ Text, Typeable a)
  => (Scope k a -> Scope k a)
  -> QName (Scope k a)
  -> Space k a
  -> Space k a
updateSpaceScope f name ns =
  ns { nsMap  = MMap.update (Just . f) name (nsMap ns) }

alterSpaceScope
  :: (e ~ Text, Typeable a)
  => (Maybe (Scope k a) -> Either e (Maybe (Scope k a)))
  -> QName (Scope k a)
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

childScopeNamesAt :: QName (Scope k a) -> Space k a -> [QName (Scope k a)]
childScopeNamesAt q ns =
  Set'.toList (GA.postSet q (nsTree ns))

_checkBusy :: QName (Scope k a) -> Space k a -> Bool
_checkBusy name ns = MMap.member name (nsMap ns)

lookupSpace :: QName a -> Space k a -> Maybe (Repr k a)
lookupSpace n s = withQScopeName n $
  \scopeName -> \case
    Nothing   -> Nothing
    Just name -> join $ lookupScope name <$> scopeAt scopeName s

spaceEntries :: Space k a -> [Repr k a]
spaceEntries ns = concatMap scopeEntries $ MMap.elems (nsMap ns)

spaceScopes :: Space k a -> [(QName (Scope k a), Scope k a)]
spaceScopes ns = MMap.toList (nsMap ns)

alterSpace
  :: (e ~ Text, Typeable a)
  => QName a
  -> Space k a
  -> (Maybe (Repr k a) -> Either e (Maybe (Repr k a)))
  -> Either e (Space k a)
alterSpace fqname ns f =
  withQScopeName fqname $
    \scopeName -> \case
      Nothing -> Left $ "Malformed QName: " <> showQName fqname
      Just name ->
        alterSpaceScope
        (failNoEntity "scope" scopeName
         $ (sequence . Just <$>)
         $ alterScope f name)
        scopeName ns

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
  { sName :: !(Name (Scope k a))
  , sMap  :: !(Map (Name a) (Repr k a))
  } deriving Generic
-- deriving instance Eq (Repr k a) => Eq (Scope k a)
-- deriving instance Ord (Repr k a) => Eq (Scope k a)
-- (Eq, Generic, Ord, Show)

scopeName :: Scope k a -> Name (Scope k a)
scopeName = sName

instance Semigroup (Scope k a) where
  Scope{sName, sMap=l} <> Scope{sMap=r}
    = Scope sName (l <> r)

emptyScope :: Name (Scope k a) -> Scope k a
emptyScope = flip Scope mempty

scope :: Name (Scope k a) -> [(Name a, Repr k a)] -> Scope k a
scope name = Scope name . Map.fromList

lookupScope :: Name a -> Scope k a -> Maybe (Repr k a)
lookupScope n s = Map.lookup n (sMap s)

scopeNames :: Scope k a -> Set (Name a)
scopeNames = keysSet . sMap

scopeEntries :: Scope k a -> [Repr k a]
scopeEntries = Map.elems . sMap

-- | Interpret a QName into its scope and name components.
withQScopeName :: QName a -> (QName (Scope k a) -> Maybe (Name a) -> b) -> b
withQScopeName (QName ns) f
  | Seq.Empty           <- ns = f mempty     Nothing
  | Seq.Empty Seq.:|> x <- ns = f mempty     (Just x)
  | xs Seq.:|> x        <- ns = f (QName $ coerceName <$> xs) (Just x)
  where coerceName :: Name a -> Name b
        coerceName = Unsafe.unsafeCoerce

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
