{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module Namespace
  ( Universe(..)
  , insertSpace
  , updateSpace
  , spaceTypes
  , someSpace
  , SomeSpace(..)
  , Space(..)
  , emptySpace
  , insertScopeAt
  , attachScopes
  , scopeAt
  , childScopeNamesAt
  , Scope(..)
  , emptyScope
  , scope
  -- * Re-exports
  , module Type
  )
where

import qualified Algebra.Graph.AdjacencyMap       as GA
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set'

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

updateSpace
  :: Universe
  -> Type
  -> (forall k a. Typeable a => Space k a -> Space k a)
  -> Universe
updateSpace u k f = Universe $ Map.update update k (uSpaceMap u)
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

scopeAt :: Space k a -> QName (Scope k a) -> Maybe (Scope k a)
scopeAt ns q = Map.lookup q (nsMap ns)

childScopeNamesAt :: QName (Scope k a) -> Space k a -> [QName (Scope k a)]
childScopeNamesAt q ns =
  Set'.toList (GA.postSet q (nsTree ns))

_checkBusy :: QName (Scope k a) -> Space k a -> Bool
_checkBusy name ns = Map.member name (nsMap ns)


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
