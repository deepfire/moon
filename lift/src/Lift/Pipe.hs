module Lift.Pipe
  ( lookupPipe
  , lookupPipeSTM
  , addPipe
  , getState
  )
where

import qualified Data.Set.Monad as Set

import qualified Control.Concurrent.STM           as STM
-- import qualified Data.Text                        as Text
import           Control.Concurrent.STM             (STM, TVar, atomically)
import qualified System.IO.Unsafe                 as Unsafe

import Basis
import Ground
import Pipe

import Lift.Hackage as Hackage
import Lift.Haskell as Haskell


initialPipeSpace :: SomePipeSpace Dynamic
initialPipeSpace
  =  Haskell.pipeSpace      (qname "Data")
  <> Hackage.pipeSpace       mempty
  <> pipeSpaceMeta

getState :: STM (SomePipeSpace Dynamic)
getState = STM.readTVar mutablePipeSpace

mutablePipeSpace :: TVar (SomePipeSpace Dynamic)
mutablePipeSpace = Unsafe.unsafePerformIO $ STM.newTVarIO initialPipeSpace
{-# NOINLINE mutablePipeSpace #-}

lookupPipe :: SomePipeSpace a -> QName Pipe -> Maybe (SomePipe a)
lookupPipe = flip lookupSpace
{-# INLINE lookupPipe #-}

lookupPipeSTM :: QName Pipe -> STM (Maybe (SomePipe Dynamic))
lookupPipeSTM name = flip lookupPipe name <$> getState

addPipe :: e ~ Text => QName Pipe -> SomePipe Dynamic -> STM (Either e Sig)
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

pipeSpaceMeta :: SomePipeSpace Dynamic
pipeSpaceMeta =
  emptyPipeSpace "Meta"
  & insertScope mempty
    (pipeScope "meta"

     -- Generators:
     --
     [ genG  "space"           TPoint' $
       STM.readTVarIO mutablePipeSpace
       <&> Right . fmap void

     , genG  "ground"          TSet' $
       pure . Right $ Set.fromList groundTypeNames

     , genG  "unit"            TPoint' $
       pure (Right ())

     -- Normal functions, lifted:
     --
     , linkG "strlen"  TPoint' TPoint' $
       \(str :: String) ->
         pure . Right $ length str

     -- Listing pipes and scopes:
     --
     , linkG "pipes"   TPoint' TSet' $
       \name ->
         STM.readTVarIO mutablePipeSpace
         <&> (scopeAt name
              >>> ((coerceName <$>) . scopeNames <$>)
              >>> maybeToEither ("No scope for name: " <> pack (show name))
              >>> \x-> x :: Either Text (Set (Name Pipe)))

     , linkG "scopes"  TPoint' TSet' $
       \name ->
         STM.readTVarIO mutablePipeSpace
         <&> Right . Set.fromList . childScopeNamesAt name

     -- Pipe sigs:
     --
     , linkG "sig"     TPoint' TPoint' $
       \name ->
         withPipePure name
         somePipeSig

     , linkG "repsig"  TPoint' TPoint' $
       \name ->
         withPipePure name
         $ somePipeSig
         >>> head . sArgs &&& sOut
         >>> join (***) tRep

     -- Listing pipes by from/to types&reps:
     --
     , linkG "to"      TPoint' TSet' $
       \name ->
         withPipe name
         $ somePipeSig >>> sOut >>> tRep
           >>> \toRep ->
                 atomically
                 $ STM.readTVar mutablePipeSpace
                   <&> Right . pipeNamesFrom (Just toRep)

     , linkG "from"    TPoint' TSet' $
       \name ->
         withPipe name
         $ somePipeSig >>> sArgs
           >>> \args ->
                 atomically $
                 if null args
                 then pure . Left $ "Pipe has no args: " <> pack (show name)
                 else Right . pipeNamesFrom (Just . tRep $ head args) <$> STM.readTVar mutablePipeSpace

     , linkG "fromrep" TPoint' TSet' $
       \case
         Nothing   -> atomically $
           Right . pipeNamesFrom Nothing <$> STM.readTVar mutablePipeSpace
         Just name -> atomically $
           case lookupNameRep name of
             Nothing -> pure . Left $ "Unknown ground: " <> pack (show name)
             Just rep ->
               Right . pipeNamesFrom (Just rep) <$> STM.readTVar mutablePipeSpace

     , linkG "torep"   TPoint' TSet' $
       \name ->
         atomically
         $ case lookupNameRep name of
             Nothing -> pure . Left $ "Unknown ground: " <> pack (show name)
             Just rep ->
               Right . pipeNamesTo rep <$> STM.readTVar mutablePipeSpace

     -- Debug:
     --
     , genG "dump"     TPoint' $ do
         putStrLn =<< unpack . showPipeSpace <$> atomically (STM.readTVar mutablePipeSpace)
         pure $ Right ()

     ])
  where
    withPipe :: QName Pipe -> (SomePipe Dynamic -> Result a) -> Result a
    withPipe name f =
      atomically (lookupPipeSTM name) >>= \case
        Nothing -> pure . Left $ "Missing pipe: " <> pack (show name)
        Just p -> f p

    withPipePure :: QName Pipe -> (SomePipe Dynamic -> a) -> Result a
    withPipePure name f =
      withPipe name $ pure . Right . f
