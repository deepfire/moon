{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ViewPatterns               #-}

module Lift.Pipe
  ( lookupPipe
  , addPipe
  )
where

import qualified Data.Map                         as Map
import           Data.Map                           (Map)
import qualified Data.Set.Monad as Set

import qualified Control.Concurrent.STM           as STM
import           Control.Concurrent.STM             (STM, TVar, atomically)
import qualified System.IO.Unsafe                 as Unsafe

import Basis
import Ground
import Pipe
import "common" Type

import Lift.Hackage as Hackage
import Lift.Haskell as Haskell


initialPipeSpace :: PipeSpace
initialPipeSpace
  =  Haskell.pipeSpace      (qname "Data")
  <> Hackage.pipeSpace       mempty
  <> pipeSpaceMeta

mutablePipeSpace :: TVar PipeSpace
mutablePipeSpace = Unsafe.unsafePerformIO $ STM.newTVarIO $ initialPipeSpace
{-# NOINLINE mutablePipeSpace #-}

lookupPipe :: QName SomePipe -> STM (Maybe SomePipe)
lookupPipe name = lookupSpace name <$> STM.readTVar mutablePipeSpace

addPipe :: e ~ Text => QName SomePipe -> SomePipe -> STM (Either e Sig)
addPipe name pipe = do
  space <- STM.readTVar mutablePipeSpace
  case lookupSpace name space of
    Just p' -> pure . Left $ "Already exists:  " <> pack (show p')
    Nothing ->
      case spaceAdd name pipe space of
        Left e -> pure $ Left e
        Right s' -> do
          STM.writeTVar mutablePipeSpace s'
          pure . Right $ somePipeSig pipe

pipeSpaceMeta :: PipeSpace
pipeSpaceMeta =
  emptyPipeSpace "Meta"
  & insertScope mempty
    (pipeScope "meta"
     [ linkG "sig"     TPoint' TPoint' $ sig
     , linkG "repsig"  TPoint' TPoint' $ repsig
     , linkG "pipes"   TPoint' TSet'   $ pipes
     , linkG "scopes"  TPoint' TSet'   $ scopes
     , genG  "ground"          TSet'   $ ground
     , linkG "from"    TPoint' TSet'   $ fromTo sOut
     , linkG "to"      TPoint' TSet'   $ fromTo sIn
     ])
  where
    sig :: QName SomePipe -> Result Sig
    sig name = do
      pipe <- atomically $ lookupPipe name
      pure $ case pipe of
        Nothing -> Left $ "Missing pipe: " <> pack (show name)
        Just (somePipeSig -> d) -> Right d
    repsig :: QName SomePipe -> Result (SomeTypeRep, SomeTypeRep)
    repsig name = do
      pipe <- atomically $ lookupPipe name
      pure $ case pipe of
        Nothing -> Left $ "Missing pipe: " <> pack (show name)
        Just (somePipeSig >>> sIn &&& sOut >>> join (***) tRep -> x) -> Right x
    pipes :: QName PipeScope -> Result (Set (Name SomePipe))
    pipes name =
      guard ("No scope for name: " <> pack (show name))
      . (scopeNames <$>) . scopeAt name <$> STM.readTVarIO mutablePipeSpace
    scopes :: QName PipeScope -> Result (Set (QName PipeScope))
    scopes name =
      Right . Set.fromList . childScopeNamesAt name <$> STM.readTVarIO mutablePipeSpace
    ground :: Result (Set Text)
    ground = pure . Right $ Set.fromList groundTypeNames
    fromTo :: (Sig -> Type) -> QName SomePipe -> Result (Set (QName SomePipe))
    fromTo sigSide name = atomically $ do
      pipe <- lookupPipe name
      case pipe of
        Nothing -> pure . Left $ "Missing pipe: " <> pack (show name)
        Just (tRep . sigSide . somePipeSig -> toRep) -> do
          Right . pipesFrom toRep <$> STM.readTVar mutablePipeSpace
