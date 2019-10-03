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
import Namespace
import Pipe
import "common" Type

import Lift.Hackage as Hackage
import Lift.Haskell as Haskell


initialPipeSpace :: Space Point SomePipe
initialPipeSpace
  =  Haskell.spacePipeData      (qname "Data")
  <> Hackage.spacePipe          mempty
  <> pipeSpaceMeta

mutablePipeSpace :: TVar (Space Point SomePipe)
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

pipeSpaceMeta :: Space Point SomePipe
pipeSpaceMeta =
  mempty
  & insertScopeAt mempty
    (pipeScope "meta"
     [ link "sig"    TPoint' TPoint' $ pipeSigPipe
     , link "pipes"  TPoint' TSet'   $ listPipeNames
     , link "scopes" TPoint' TSet'   $ listPipeScopes
     , gen  "gnd"            TSet'   $ listGround
     ])
  where
    pipeSigPipe :: QName SomePipe -> Result Sig
    pipeSigPipe name = do
      pipe <- atomically $ lookupPipe name
      pure $ case pipe of
        Nothing -> Left $ "Missing pipe: " <> pack (show name)
        Just (somePipeSig -> d) -> Right d
    listPipeNames :: QName (Scope Point SomePipe) -> Result (Set (Name SomePipe))
    listPipeNames name =
      guard ("No scope for name: " <> pack (show name))
      . (scopeNames <$>) . scopeAt name <$> STM.readTVarIO mutablePipeSpace
    listPipeScopes :: QName (Scope Point SomePipe) -> Result (Set (QName (Scope Point SomePipe)))
    listPipeScopes name =
      Right . Set.fromList . childScopeNamesAt name <$> STM.readTVarIO mutablePipeSpace
    listGround :: Result (Set Text)
    listGround = pure . Right $ Set.fromList groundTypeNames
