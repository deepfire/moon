{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE ViewPatterns               #-}

module Pipe.Space
  ( PipeSpace
  , emptyPipeSpace
  , pipesFrom
  , pipesTo
  , insertScope
  , attachScopes
  , scopeAt
  , childScopeNamesAt
  , lookupSpace
  , spaceAdd
  -- * ...
  , pipeIndexElems
  , pipeIndexPower
  -- * Re-exports
  , module Pipe.Scope
  )
where

import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import qualified Data.Text                        as Text
import           Data.Monoid.Generic
import           GHC.Generics                       (Generic)

import Basis
import Namespace (Space)
import qualified Namespace
import Pipe.Scope
import Pipe.Types
import Type

data PipeSpace = PipeSpace
  { psName  :: QName (PipeSpace)
  , psSpace :: !(Space Point SomePipe)
  , psFrom  :: !(MonoidalMap SomeTypeRep (Set (QName SomePipe)))
  , psTo    :: !(MonoidalMap SomeTypeRep (Set (QName SomePipe)))
  } deriving Generic
    deriving Semigroup via GenericSemigroup PipeSpace

emptyPipeSpace :: Name PipeSpace -> PipeSpace
emptyPipeSpace n = PipeSpace
  { psName  = qname n
  , psSpace = Namespace.emptySpace
  , psFrom  = mempty
  , psTo    = mempty
  }

pipesFrom :: SomeTypeRep -> PipeSpace -> Set (QName SomePipe)
pipesFrom str =
  fromMaybe mempty . MMap.lookup str . psFrom

pipesTo :: SomeTypeRep -> PipeSpace -> Set (QName SomePipe)
pipesTo str =
  fromMaybe mempty . MMap.lookup str . psTo

type PipeRepIndex = MonoidalMap SomeTypeRep (Set (QName SomePipe))

insertScope :: QName PipeScope -> PipeScope -> PipeSpace -> PipeSpace
insertScope prefix scope ps =
  ps { psSpace = Namespace.insertScope prefix scope (psSpace ps)
     , psFrom  = psFrom'
     , psTo    = psTo'
     }
 where
   fro, to, psFrom', psTo' :: PipeRepIndex
   (,) fro to =  scopeIndices prefix scope
   psFrom' = psFrom ps <> fro
   psTo'   = psTo   ps <> to

   _showPRI :: PipeRepIndex -> Text
   _showPRI map =
     Text.intercalate ", " $
     MMap.foldlWithKey (\ss str qs-> pack (printf "%s -> (%s)" (show str) (Text.intercalate ", " $ showQName <$> Set.toList qs)) : ss) [] map


pipeIndexElems :: PipeRepIndex -> [QName SomePipe]
pipeIndexElems = MMap.elems >>> (Set.elems <$>) >>> mconcat

pipeIndexPower :: PipeRepIndex -> Int
pipeIndexPower = length . pipeIndexElems

scopeIndices :: QName PipeScope -> PipeScope -> (PipeRepIndex, PipeRepIndex)
scopeIndices prefix =
  Namespace.scopeEntries
  >>> ((<&> pipeEdge prefix sIn)
       &&&
       (<&> pipeEdge prefix sOut))
  >>> join (***) mconcat
 where
   pipeEdge :: QName PipeScope -> (Sig -> Type) -> SomePipe -> PipeRepIndex
   pipeEdge prefix selector =
     (somePipeSig  >>> selector >>> tRep)
     &&&
     (somePipeName >>> (coerceQName prefix |>) >>> Set.singleton)
     >>> uncurry MMap.singleton

attachScopes :: QName PipeScope -> [PipeScope] -> PipeSpace -> PipeSpace
attachScopes sub scopes ns = foldr (insertScope sub) ns scopes

scopeAt :: QName PipeScope -> PipeSpace -> Maybe PipeScope
scopeAt q ps = Namespace.scopeAt q (psSpace ps)

childScopeNamesAt :: QName PipeScope -> PipeSpace -> [QName PipeScope]
childScopeNamesAt q ps = Namespace.childScopeNamesAt q (psSpace ps)

lookupSpace :: QName SomePipe -> PipeSpace -> Maybe SomePipe
lookupSpace q ps = Namespace.lookupSpace q (psSpace ps)

spaceAdd
  :: forall e. (e ~ Text)
  => QName SomePipe
  -> SomePipe
  -> PipeSpace -> Either e PipeSpace
spaceAdd name x ps =
  PipeSpace
   <$> pure (psName ps)
   <*> Namespace.spaceAdd name x (psSpace ps)
   <*> pure (MMap.alter (alteration name) strFrom $ psFrom ps)
   <*> pure (MMap.alter (alteration name) strTo   $ psTo ps)
 where
   strFrom, strTo :: SomeTypeRep
   (,) strFrom strTo = join (***) tRep . (sIn &&& sOut) $ somePipeSig x

   alteration
     :: QName SomePipe
     -> (   Maybe (Set (QName SomePipe))
         -> Maybe (Set (QName SomePipe)))
   alteration new = Just . maybe (Set.singleton new) (Set.insert new)
