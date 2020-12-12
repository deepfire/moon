module Dom.Scope.SomePipe (module Dom.Scope.SomePipe) where

import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set

import Basis

import Dom.Name
import Dom.Pipe
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Sig
import Dom.SomeType


--------------------------------------------------------------------------------
-- * Scope of opaque pipes
--
type SomePipeScope p = PointScope (SomePipe p)

emptyPipeScope :: Name Scope -> SomePipeScope p
emptyPipeScope = emptyScope . coerceName

pipeScope :: Name Scope -> [SomePipe p] -> SomePipeScope p
pipeScope name pipes = scope (coerceName name) $
  zip (coerceName . somePipeName <$> pipes) pipes

scopeIndices
  :: PointScope (SomePipe p)
  -> ( MonoidalMap (Maybe SomeTypeRep) (Set (QName Pipe))
     , MonoidalMap        SomeTypeRep  (Set (QName Pipe)))
scopeIndices =
  scopeEntries
  >>> ((<&> (\sp-> pipeEdge sp $ if null (sArgs $ somePipeSig sp)
                                 then const Nothing
                                 else Just . tRep . unI . head . sArgs))
       &&&
       (<&> (\sp-> pipeEdge sp $ tRep . unI . sOut)))
  >>> (mconcat *** mconcat)
 where
   pipeEdge :: Show a
            => SomePipe p -> (ISig -> a)
            -> MonoidalMap a (Set (QName Pipe))
   pipeEdge sp sigKey =
     sp &
     ((somePipeSig  >>> sigKey)
      &&&
      (somePipeQName
       >>> Set.singleton)
      >>> (\(k, v) -> -- XXX:
                      -- traceErr (mconcat [show (head $ toList v), ": ", show k])
                      (k, v))
      >>> uncurry MMap.singleton)

pipeIndexElems :: MonoidalMap SomeTypeRep (Set (QName Pipe)) -> [QName Pipe]
pipeIndexElems = MMap.elems >>> (Set.elems <$>) >>> mconcat
