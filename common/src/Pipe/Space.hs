module Pipe.Space
  ( SomePipeSpace
  , PipeSpace
  , emptyPipeSpace
  , pipesFrom
  , pipesTo
  -- , pipeDescs
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

import Basis
import Namespace (Scope)
import qualified Namespace
import Pipe.Scope
import Pipe.Types
import Type


emptyPipeSpace :: Name PipeSpace -> SomePipeSpace
emptyPipeSpace n = PipeSpace
  { psName  = qname n
  , psSpace = Namespace.emptySpace
  , psFrom  = mempty
  , psTo    = mempty
  }

-- pipeDescs :: PipeSpace a -> Set a
-- pipeDescs = Set.fromList . (somePipeDesc <$>) . Namespace.spaceEntries . psSpace

pipesFrom :: SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipesFrom str =
  fromMaybe mempty . MMap.lookup str . psFrom

pipesTo :: SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipesTo str =
  fromMaybe mempty . MMap.lookup str . psTo

type PipeRepIndex = MonoidalMap SomeTypeRep (Set (QName Pipe))

insertScope :: QName Scope -> SomePipeScope -> SomePipeSpace -> SomePipeSpace
insertScope prefix scope ps =
  ps { psSpace = Namespace.insertScope (coerceQName prefix) scope (psSpace ps)
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


pipeIndexElems :: PipeRepIndex -> [QName Pipe]
pipeIndexElems = MMap.elems >>> (Set.elems <$>) >>> mconcat

pipeIndexPower :: PipeRepIndex -> Int
pipeIndexPower = length . pipeIndexElems

scopeIndices :: QName Scope -> PipeScope SomePipe -> (PipeRepIndex, PipeRepIndex)
scopeIndices prefix =
  Namespace.scopeEntries
  >>> ((<&> pipeEdge prefix sIn)
       &&&
       (<&> pipeEdge prefix sOut))
  >>> join (***) mconcat
 where
   pipeEdge :: QName Scope -> (Sig -> Type) -> SomePipe -> PipeRepIndex
   pipeEdge prefix selector =
     (somePipeSig  >>> selector >>> tRep)
     &&&
     (somePipeName >>> (coerceQName prefix |>) >>> Set.singleton)
     >>> uncurry MMap.singleton

attachScopes :: QName Scope -> [SomePipeScope] -> SomePipeSpace -> SomePipeSpace
attachScopes sub scopes ns = foldr (insertScope sub) ns scopes

scopeAt :: QName Scope -> PipeSpace a -> Maybe (PipeScope a)
scopeAt q ps = Namespace.scopeAt (coerceQName q) (psSpace ps)

childScopeNamesAt :: QName Scope -> PipeSpace a -> [QName Scope]
childScopeNamesAt q ps = coerceQName <$>
  Namespace.childScopeNamesAt (coerceQName q) (psSpace ps)

lookupSpace :: QName Pipe -> PipeSpace a -> Maybe a
lookupSpace q ps = Namespace.lookupSpace (coerceQName q) (psSpace ps)

spaceAdd
  :: forall e. (e ~ Text)
  => QName Pipe
  -> SomePipe
  -> SomePipeSpace -> Either e SomePipeSpace
spaceAdd name x ps =
  PipeSpace
   <$> pure (psName ps)
   <*> Namespace.spaceAdd (coerceQName name) x (psSpace ps)
   <*> pure (MMap.alter (alteration name) strFrom $ psFrom ps)
   <*> pure (MMap.alter (alteration name) strTo   $ psTo ps)
 where
   strFrom, strTo :: SomeTypeRep
   (,) strFrom strTo = join (***) tRep . (sIn &&& sOut) $ somePipeSig x

   alteration
     :: QName Pipe
     -> (   Maybe (Set (QName Pipe))
         -> Maybe (Set (QName Pipe)))
   alteration new = Just . maybe (Set.singleton new) (Set.insert new)
