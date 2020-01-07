module Pipe.Space
  ( SomePipeSpace
  , PipeSpace
  , emptyPipeSpace
  , pipesFrom
  , pipesTo
  , pipeNamesFrom
  , pipeNamesTo
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
import Namespace (PointScope, Scope)
import qualified Namespace
import Pipe.Scope
import Pipe.Types
import Type


emptyPipeSpace :: Name PipeSpace -> SomePipeSpace p
emptyPipeSpace n = PipeSpace
  { psName  = qname n
  , psSpace = Namespace.emptySpace
  , psFrom  = mempty
  , psTo    = mempty
  }

pipesFrom :: SomeTypeRep -> PipeSpace a -> [a]
pipesFrom str spc = setToList (pipeNamesFrom str spc) &
  mapMaybe (flip lookupSpace spc . coerceQName)

pipesTo :: SomeTypeRep -> PipeSpace a -> [a]
pipesTo str spc = setToList (pipeNamesTo str spc) &
  mapMaybe (flip lookupSpace spc . coerceQName)

pipeNamesFrom :: SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipeNamesFrom str =
  fromMaybe mempty . MMap.lookup str . psFrom

pipeNamesTo :: SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipeNamesTo str =
  fromMaybe mempty . MMap.lookup str . psTo

type PipeRepIndex p = MonoidalMap SomeTypeRep (Set (QName Pipe))

insertScope :: QName Scope -> SomePipeScope p -> SomePipeSpace p -> SomePipeSpace p
insertScope prefix scope ps =
  ps { psSpace = Namespace.insertScope (coerceQName prefix) scope (psSpace ps)
     , psFrom  = psFrom'
     , psTo    = psTo'
     }
 where
   fro, to, psFrom', psTo' :: PipeRepIndex p
   (,) fro to =  scopeIndices (prefix <> (qname $ Namespace.scopeName scope)) scope
   psFrom' = psFrom ps <> fro
   psTo'   = psTo   ps <> to

   _showPRI :: PipeRepIndex p -> Text
   _showPRI map =
     Text.intercalate ", " $
     MMap.foldlWithKey (\ss str qs-> pack (printf "%s -> (%s)" (show str) (Text.intercalate ", " $ showQName <$> Set.toList qs)) : ss) [] map


pipeIndexElems :: PipeRepIndex p -> [QName Pipe]
pipeIndexElems = MMap.elems >>> (Set.elems <$>) >>> mconcat

pipeIndexPower :: PipeRepIndex p -> Int
pipeIndexPower = length . pipeIndexElems

scopeIndices
  :: QName Scope
  -> PointScope (SomePipe p)
  -> (PipeRepIndex p, PipeRepIndex p)
scopeIndices prefix =
  Namespace.scopeEntries
  >>> ((<&> pipeEdge prefix sIn)
       &&&
       (<&> pipeEdge prefix sOut))
  >>> join (***) mconcat
 where
   pipeEdge :: QName Scope -> (Sig -> Type) -> SomePipe p -> PipeRepIndex p
   pipeEdge prefix selector =
     (somePipeSig  >>> selector >>> tRep)
     &&&
     (somePipeName >>> (coerceQName prefix |>)
      >>> Set.singleton)
     >>> uncurry MMap.singleton

attachScopes :: QName Scope -> [SomePipeScope p] -> SomePipeSpace p -> SomePipeSpace p
attachScopes sub scopes ns = foldr (insertScope sub) ns scopes

scopeAt :: QName Scope -> PipeSpace a -> Maybe (PointScope a)
scopeAt q ps = Namespace.scopeAt (coerceQName q) (psSpace ps)

childScopeNamesAt :: QName Scope -> PipeSpace a -> [QName Scope]
childScopeNamesAt q ps = coerceQName <$>
  Namespace.childScopeNamesAt (coerceQName q) (psSpace ps)

lookupSpace :: QName Pipe -> PipeSpace a -> Maybe a
lookupSpace q ps = Namespace.lookupSpace (coerceQName q) (psSpace ps)

spaceAdd
  :: forall e p. (e ~ Text, Typeable p)
  => QName Pipe
  -> SomePipe p
  -> SomePipeSpace p -> Either e (SomePipeSpace p)
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
