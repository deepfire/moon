module Dom.Space.Pipe (module Dom.Space.Pipe) where

import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import qualified Data.Text                        as Text

import Basis

import Dom.CTag
import Dom.Name
import Dom.Pipe
import Dom.Scope
import Dom.Sig
import Dom.Pipe.SomePipe
import Dom.Space


--------------------------------------------------------------------------------
-- * Space of Pipes, indexed by endpoints
--
data PipeSpace a = PipeSpace
  { psName  :: !(QName PipeSpace)
  , psSpace :: !(Space Point a)
  , psFrom  :: !(MonoidalMap (Maybe SomeTypeRep) (Set (QName Pipe)))
  , psTo    :: !(MonoidalMap        SomeTypeRep  (Set (QName Pipe)))
  }
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- * Projections
--
lookupPipeSpace :: QName Pipe -> PipeSpace a -> Maybe a
lookupPipeSpace q ps = lookupSpace (coerceQName q) (psSpace ps)

pipesFromCstr :: PipeSpace a -> Maybe SomeTypeRep -> [a]
pipesFromCstr spc Nothing  = spaceEntries (psSpace spc)
pipesFromCstr spc (Just x) = pipesFrom spc (Just x)

pipesToCstr :: PipeSpace a -> Maybe SomeTypeRep -> [a]
pipesToCstr spc Nothing  = spaceEntries (psSpace spc)
pipesToCstr spc (Just x) = pipesTo spc x

pipesFrom :: PipeSpace a -> Maybe SomeTypeRep -> [a]
pipesFrom spc mStr = setToList (pipeNamesFrom mStr spc) &
  mapMaybe (flip lookupPipeSpace spc . coerceQName)
  --traceErr (mconcat ["pipesFrom ", show mStr, " -> ", show (length xs)])

pipesTo :: PipeSpace a -> SomeTypeRep -> [a]
pipesTo spc str = setToList (pipeNamesTo str spc) &
  mapMaybe (flip lookupPipeSpace spc . coerceQName)

pipeNamesFrom :: Maybe SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipeNamesFrom str =
  psFrom >>> MMap.lookup str >>> fromMaybe mempty
  --traceErr (mconcat ["pipeNamesFrom ", show str, " -> ", show (length xs)])

pipeNamesTo :: SomeTypeRep -> PipeSpace a -> Set (QName Pipe)
pipeNamesTo   str =
  psTo >>> MMap.lookup str >>> fromMaybe mempty

--------------------------------------------------------------------------------
-- * Scope ops
--
pipeScopeAt :: QName Scope -> PipeSpace a -> Maybe (PointScope a)
pipeScopeAt q ps = scopeAt (coerceQName q) (psSpace ps)

childPipeScopeQNamesAt :: QName Scope -> PipeSpace a -> [QName Scope]
childPipeScopeQNamesAt q ps = coerceQName <$>
  childScopeQNamesAt (coerceQName q) (psSpace ps)

--------------------------------------------------------------------------------
-- * Instances
--
instance Functor PipeSpace where
  fmap f ps@PipeSpace{psSpace} =
    ps { psSpace = f <$> psSpace }

-- Caveat:  non-law-abiding.
instance Applicative PipeSpace where
  pure x = PipeSpace mempty (pure x) mempty mempty
  PipeSpace _ f _ _ <*> PipeSpace n x fro to =
    PipeSpace n (f <*> x) fro to

instance Foldable PipeSpace where
  foldMap toM PipeSpace{..} = foldMap toM psSpace

instance Traversable PipeSpace where
  traverse f s@PipeSpace{..} =
    traverse f psSpace <&> \x -> s { psSpace = x }

instance (Eq a, Serialise a, Typeable a) => Serialise (PipeSpace a) where
  encode PipeSpace{psName, psSpace, psFrom, psTo} =
    encodeListLen 5
    <> encodeWord 2177
    <> encode psName
    <> encode psSpace
    <> encode (MMap.map setToList psFrom)
    <> encode (MMap.map setToList psTo)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (5, 2177) -> do
        name <- decode
        spc <- decode
        PipeSpace
          <$> pure name
          <*> pure spc
          <*> (MMap.map Set.fromList <$> decode)
          <*> (MMap.map Set.fromList <$> decode)
      _ -> fail $ "PipeSpace failed decode: "
           <>" len="<> show len
           <>" tag="<> show tag
           <>" rep="<> show (typeRep @(PipeSpace a))

instance Semigroup (PipeSpace a) where
  l <> r = PipeSpace
    { psName  = psName  l
    , psSpace = psSpace l <> psSpace r
    , psFrom  = psFrom  l <> psFrom  r
    , psTo    = psTo    l <> psTo    r
    }

instance Monoid (PipeSpace a) where
  mempty = PipeSpace mempty mempty mempty mempty

instance Typeable a => Read (PipeSpace a) where readPrec = failRead

instance Show (PipeSpace a) where
  show PipeSpace{psName, psSpace} =
    "(PipeSpace "<>show psName<>" "<>show (length $ spaceEntries psSpace)<>" entries)"

--------------------------------------------------------------------------------
-- * Utils
--
showPipeSpace :: PipeSpace (SomePipe a) -> Text
showPipeSpace PipeSpace{..} =
  Text.intercalate "\n"
    ( mconcat ["PipeSpace ", pack (show psName)]
    : toList (showElt <$> spaceEntries psSpace))
 where
   showElt sp = Text.concat
     [ "  ", showQName $ somePipeQName sp
     , ": ", showSig $ somePipeSig sp
     ]
