module Dom.Space.SomePipe (module Dom.Space.SomePipe) where

import           Codec.Serialise
import           Data.Map.Monoidal.Strict           (MonoidalMap)
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import qualified Data.Text                        as Text
import           Type.Reflection                    (typeRep)

import Basis

import Dom.Name
import Dom.Pipe
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Scope.SomePipe
import Dom.Sig
import Dom.Space
import Dom.Space.Pipe
import Dom.SomeType


--------------------------------------------------------------------------------
-- * Space of opaque 'Pipe's
--
type SomePipeSpace p = PipeSpace (SomePipe p)

--------------------------------------------------------------------------------
-- * Constructors
--
emptySomePipeSpace :: Name PipeSpace -> SomePipeSpace p
emptySomePipeSpace n = PipeSpace
  { psName  = qname n
  , psSpace = emptySpace
  , psFrom  = mempty
  , psTo    = mempty
  }

--------------------------------------------------------------------------------
-- * Point ops
--
spsAdd ::
     forall e p. (e ~ Text, Typeable p, Ord (SomePipe p))
  => QName Pipe
  -> SomePipe p
  -> SomePipeSpace p -> Either e (SomePipeSpace p)
spsAdd name x ps = do
  spc <- spaceAdd (coerceQName name) (somePipeSetQName name x) (psSpace ps)
  pure PipeSpace
    { psName  = psName ps
    , psSpace = spc
    , psFrom  = MMap.alter (alteration name)
                  (Just $ if null . sArgs $ somePipeSig x then strTo else strFrom)
                  (psFrom ps)
    , psTo    = MMap.alter (alteration name)
                  strTo
                  (psTo ps)
    }
 where
   strFrom, strTo :: SomeTypeRep
   (,) strFrom strTo = join (***) (tRep . unI) . (head . sArgs &&& sOut) $ somePipeSig x

   alteration
     :: QName Pipe
     -> (   Maybe (Set (QName Pipe))
         -> Maybe (Set (QName Pipe)))
   alteration new = Just . maybe (Set.singleton new) (Set.insert new)

--------------------------------------------------------------------------------
-- * Scope ops
--
spsAttachScopes ::
  QName Scope -> [SomePipeScope p] -> SomePipeSpace p -> SomePipeSpace p
spsAttachScopes sub scopes ns = foldr (spsInsertScope sub) ns scopes

spsInsertScope :: QName Scope -> SomePipeScope p -> SomePipeSpace p -> SomePipeSpace p
spsInsertScope pfx sc =
  spsInsertScopeAt
    (coerceQName pfx `append` coerceName (scopeName sc))
    sc

spsInsertScopeAt :: QName Scope -> SomePipeScope p -> SomePipeSpace p -> SomePipeSpace p
spsInsertScopeAt pfx scop ps =
  ps { psSpace = insertScopeAt (coerceQName pfx) scop' (psSpace ps)
     , psFrom  = psFrom ps <> fro
     , psTo    = psTo   ps <> to
     }
 where
   scop' = mapScope
             (\p ->
                somePipeSetQName (coerceQName pfx `append` somePipeName p) p)
             scop
   fro :: MonoidalMap (Maybe SomeTypeRep) (Set (QName Pipe))
   to  :: MonoidalMap        SomeTypeRep  (Set (QName Pipe))
   (,) fro to = scopeIndices scop'

   _showPRI :: MonoidalMap SomeTypeRep (Set (QName Pipe)) -> Text
   _showPRI index =
     Text.intercalate ", " $
     MMap.foldlWithKey (\ss str qs-> pack (printf "%s -> (%s)" (show str) (Text.intercalate ", " $ showQName <$> Set.toList qs)) : ss) [] index
