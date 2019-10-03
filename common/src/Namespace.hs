{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module Namespace
  ( Universe(..)
  , insertSpace
  , updateUniverse
  , spaceTypes
  , someSpace
  , SomeSpace(..)
  , Space(..)
  , emptySpace
  , insertScopeAt
  , attachScopes
  , scopeAt
  , childScopeNamesAt
  , updateSpaceScope
  , lookupSpace
  , alterSpace
  , spaceAdd
  , spaceUpdate
  , Scope
  , emptyScope
  , scope
  , lookupScope
  , scopeNames
  , withQScopeName
  , updateScope
  , alterScope
  -- * Re-exports
  , module Type
  )
where

import qualified Algebra.Graph.AdjacencyMap       as GA
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set'
import qualified Data.Sequence                    as Seq

import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Type

--------------------------------------------------------------------------------
-- * Namespacing
--
data Universe = Universe
  { uSpaceMap :: !(Map Type SomeSpace)
  }

insertSpace
  :: forall k a. (Typeable k, Typeable a)
  => Space k a
  -> Universe -> Universe
insertSpace s u = Universe $ Map.insert ty (SomeSpace ty s) (uSpaceMap u)
  where ty = proxyType (Proxy @k) (Proxy @a)

updateUniverse
  :: (forall k a. Typeable a => Space k a -> Space k a)
  -> Type
  -> Universe
  -> Universe
updateUniverse f k u = Universe $ Map.update update k (uSpaceMap u)
  where update SomeSpace{ssType, ssSpace} =
          Just $ SomeSpace ssType (f ssSpace)

spaceTypes :: Universe -> Set Type
spaceTypes u = keysSet . uSpaceMap $ u

someSpace :: Universe -> Type -> Maybe SomeSpace
someSpace u k = Map.lookup k (uSpaceMap u)


data SomeSpace where
  SomeSpace :: (Typeable k, Typeable a) =>
    { ssType  :: !Type
    , ssSpace :: !(Space k a)
    } -> SomeSpace

-- someSpace :: forall a. Typeable a => Space a -> SomeSpace
-- someSpace s = SomeSpace (R.someTypeRep (Proxy @a)) s

-- ssName :: SomeSpace -> Name SomeSpace
-- ssName SomeSpace{ssSpace=(Space{nsName=(Name s)})} = Name s


data Space k a = Space
  { nsTree :: !(GA.AdjacencyMap (QName (Scope k a)))
  , nsMap  :: !(Map             (QName (Scope k a)) (Scope k a))
  }

instance Semigroup (Space k a) where
  Space{nsTree=lt, nsMap=lm} <> Space{nsTree=rt, nsMap=rm}
    = Space (lt `GA.overlay` rt) (lm <> rm)

instance Monoid (Space k a) where
  mempty = emptySpace

emptySpace :: Space k a
emptySpace = Space
  { nsTree = GA.vertex mempty
  , nsMap  = Map.fromList [(mempty, emptyScope "")]
  } where

insertScopeAt :: QName (Scope k a) -> Scope k a -> Space k a -> Space k a
insertScopeAt prefix scope ns =
  ns { nsMap  = Map.insert name scope $ nsMap ns
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
  foldr (uncurry insertScopeAt) ns $
        zip names scopes
 where
  names = append sub . sName <$> scopes

scopeAt :: QName (Scope k a) -> Space k a -> Maybe (Scope k a)
scopeAt q ns = Map.lookup q (nsMap ns)

updateSpaceScope
  :: (e ~ Text, Typeable a)
  => (Scope k a -> Scope k a)
  -> QName (Scope k a)
  -> Space k a
  -> Space k a
updateSpaceScope f name ns =
  ns { nsMap  = Map.update (Just . f) name (nsMap ns) }

alterSpaceScope
  :: (e ~ Text, Typeable a)
  => (Maybe (Scope k a) -> Either e (Maybe (Scope k a)))
  -> QName (Scope k a)
  -> Space k a
  -> Either e (Space k a)
alterSpaceScope f name ns =
  (\m -> ns { nsMap = m }) <$> Map.alterF f name (nsMap ns)

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
_failHasEntity ty name _ (Just x) = Left $ "Already has "<>ty<>": " <> showQName name
_failHasEntity _  _    x  Nothing = Just <$> x

childScopeNamesAt :: QName (Scope k a) -> Space k a -> [QName (Scope k a)]
childScopeNamesAt q ns =
  Set'.toList (GA.postSet q (nsTree ns))

_checkBusy :: QName (Scope k a) -> Space k a -> Bool
_checkBusy name ns = Map.member name (nsMap ns)

lookupSpace :: QName a -> Space k a -> Maybe (Repr k a)
lookupSpace n s = withQScopeName n $
  \scopeName -> \case
    Nothing   -> Nothing
    Just name -> join $ lookupScope name <$> scopeAt scopeName s

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
      Just _ -> Left $ "Already has element: " <> showQName name)

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
  }
-- deriving instance Eq (Repr k a) => Eq (Scope k a)
-- deriving instance Ord (Repr k a) => Eq (Scope k a)
-- (Eq, Generic, Ord, Show)

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
