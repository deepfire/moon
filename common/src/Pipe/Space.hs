module Pipe.Space
  ( SomePipeSpace
  , PipeSpace
  , emptyPipeSpace
  , pipesFromCstr
  , pipesFrom
  , pipesTo
  , pipeNamesFrom
  , pipeNamesTo
  , showPipeSpace
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
import Namespace (PointScope)
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

showPipeSpace :: PipeSpace (SomePipe a) -> Text
showPipeSpace PipeSpace{..} =
  Text.intercalate "\n"
    ( mconcat ["PipeSpace ", pack (show psName)]
    : toList (showElt <$> Namespace.spaceEntries psSpace))
 where
   showElt sp = Text.concat
     [ "  ", showName $ somePipeName sp
     , ": ", showSig $ somePipeSig sp
     ]

pipesFromCstr :: PipeSpace a -> Maybe SomeTypeRep -> [a]
pipesFromCstr spc Nothing  = Namespace.spaceEntries (psSpace spc)
pipesFromCstr spc (Just x) = pipesFrom spc (Just x)

pipesFrom :: PipeSpace a -> Maybe SomeTypeRep -> [a]
pipesFrom spc mStr = setToList (pipeNamesFrom mStr spc) &
  mapMaybe (flip lookupSpace spc . coerceQName) &
  (\xs -> traceErr (mconcat ["pipesFrom ", show mStr, " -> ", show (length xs)])
    xs)

pipesTo :: PipeSpace a -> SomeTypeRep -> [a]
pipesTo spc str = setToList (pipeNamesTo str spc) &
  mapMaybe (flip lookupSpace spc . coerceQName)

pipeNamesFrom :: Maybe SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipeNamesFrom str = psFrom >>> MMap.lookup str >>> fromMaybe mempty
                    >>> (\xs -> traceErr (mconcat ["pipeNamesFrom ", show str, " -> ", show (length xs)])
                          xs)

pipeNamesTo :: SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipeNamesTo   str = psTo   >>> MMap.lookup str >>> fromMaybe mempty

insertScope :: QName Scope -> SomePipeScope p -> SomePipeSpace p -> SomePipeSpace p
insertScope pfx scop ps =
  ps { psSpace = Namespace.insertScope (coerceQName pfx) scop (psSpace ps)
     , psFrom  = psFrom ps <> fro
     , psTo    = psTo   ps <> to
     }
 where
   fro :: MonoidalMap (Maybe SomeTypeRep) (Set (QName Pipe))
   to  :: MonoidalMap        SomeTypeRep  (Set (QName Pipe))
   (,) fro to = scopeIndices (pfx <> qname (Namespace.scopeName scop)) scop

   _showPRI :: MonoidalMap SomeTypeRep (Set (QName Pipe)) -> Text
   _showPRI index =
     Text.intercalate ", " $
     MMap.foldlWithKey (\ss str qs-> pack (printf "%s -> (%s)" (show str) (Text.intercalate ", " $ showQName <$> Set.toList qs)) : ss) [] index


pipeIndexElems :: MonoidalMap SomeTypeRep (Set (QName Pipe)) -> [QName Pipe]
pipeIndexElems = MMap.elems >>> (Set.elems <$>) >>> mconcat

pipeIndexPower :: MonoidalMap SomeTypeRep (Set (QName Pipe)) -> Int
pipeIndexPower = length . pipeIndexElems

scopeIndices
  :: QName Scope
  -> PointScope (SomePipe p)
  -> ( MonoidalMap (Maybe SomeTypeRep) (Set (QName Pipe))
     , MonoidalMap        SomeTypeRep  (Set (QName Pipe)))
scopeIndices prefix =
  Namespace.scopeEntries
  >>> ((<&> (\sp-> pipeEdge sp prefix $ if null (sArgs $ somePipeSig sp)
                                        then const Nothing
                                        else Just . tRep . head . sArgs))
       &&&
       (<&> (\sp-> pipeEdge sp prefix $ tRep . sOut)))
  >>> (mconcat *** mconcat)
 where
   pipeEdge :: Show a
            => SomePipe p -> QName Scope -> (Sig -> a)
            -> MonoidalMap a (Set (QName Pipe))
   pipeEdge sp pfx sigKey =
     sp &
     ((somePipeSig  >>> sigKey)
      &&&
      (somePipeName >>> (coerceQName pfx `append`)
       >>> Set.singleton)
      >>> (\(k, v) -> traceErr (mconcat [show (head $ toList v), ": ", show k])
                      (k, v))
      >>> uncurry MMap.singleton)

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
   <*> pure (MMap.alter (alteration name)
             (Just $ if null . sArgs $ somePipeSig x then strTo else strFrom)
             (psFrom ps))
   <*> pure (MMap.alter (alteration name) strTo   $ psTo ps)
 where
   strFrom, strTo :: SomeTypeRep
   (,) strFrom strTo = join (***) tRep . (head . sArgs &&& sOut) $ somePipeSig x

   alteration
     :: QName Pipe
     -> (   Maybe (Set (QName Pipe))
         -> Maybe (Set (QName Pipe)))
   alteration new = Just . maybe (Set.singleton new) (Set.insert new)
