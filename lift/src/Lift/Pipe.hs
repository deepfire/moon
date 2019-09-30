{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ViewPatterns               #-}

module Lift.Pipe
  ( lookupPipe
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
  =   Ground.spacePipe          mempty
  <> Haskell.spacePipeData      (qname "Data")
  <> Hackage.spacePipe          mempty

pipeSpace :: TVar (Space Point SomePipe)
pipeSpace = Unsafe.unsafePerformIO $ STM.newTVarIO $ initialPipeSpace
{-# NOINLINE pipeSpace #-}

lookupPipe :: QName SomePipe -> STM (Maybe SomePipe)
lookupPipe name = lookupSpace name <$> STM.readTVar pipeSpace

pipeSpaceMeta :: Space Point SomePipe
pipeSpaceMeta =
  mempty
  & insertScopeAt mempty
    (pipeScope ""
     [ linkG "sig" TPoint TPoint $ pipeSigPipe
     -- , genG "pipes" TSet $ listPipeNames
     ])
  where
    pipeSigPipe :: QName SomePipe -> Result Sig
    pipeSigPipe name = do
      pipe <- atomically $ lookupPipe name
      pure $ case pipe of
        Nothing -> Left $ "Missing: " <> pack (show name)
        Just (somePipeSig -> d) -> Right d
    -- listPipeNames :: Result (Set (Name Pipe))
    -- listPipeNames = Right . Set.fromList . Map.keys <$> STM.readTVarIO pipes
