module Lift.Pipe (module Lift.Pipe) where

import qualified Data.Set.Monad                   as Set
import qualified Data.Text                        as Text

import qualified Control.Concurrent.STM           as STM
-- import qualified Data.Text                        as Text
import           Control.Concurrent.STM             (STM, TVar, atomically)
import qualified System.IO.Unsafe                 as Unsafe

import Basis

import Dom.CTag
import Dom.Error
import Dom.Ground
import Dom.Name
import Dom.Pipe
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Scope.SomePipe
import Dom.Sig
import Dom.SomeType
import Dom.Space.Pipe
import Dom.Space.SomePipe

import Lift.Hackage as Hackage
import Lift.Haskell as Haskell


initialPipeSpace :: SomePipeSpace Dynamic
initialPipeSpace
  =  Haskell.pipeSpace      (qname "Data")
  <> Hackage.pipeSpace       mempty
  <> rootPipeSpace

getState :: STM (SomePipeSpace Dynamic)
getState = STM.readTVar mutablePipeSpace

mutablePipeSpace :: TVar (SomePipeSpace Dynamic)
mutablePipeSpace = Unsafe.unsafePerformIO $ STM.newTVarIO initialPipeSpace
{-# NOINLINE mutablePipeSpace #-}

lookupSomePipe :: SomePipeSpace a -> QName Pipe -> Maybe (SomePipe a)
lookupSomePipe = flip lookupPipeSpace
{-# INLINE lookupSomePipe #-}

lookupPipeSTM :: QName Pipe -> STM (Maybe (SomePipe Dynamic))
lookupPipeSTM name = lookupPipeSpace name <$> getState

addPipe :: e ~ Text => QName Pipe -> SomePipe Dynamic -> STM (Either e ISig)
addPipe name pipe = do
  space <- STM.readTVar mutablePipeSpace
  case lookupPipeSpace name space of
    Just p' -> pure . Left $ "Already exists:  " <> pack (show p')
    Nothing ->
      case spsAdd name pipe space of
        Left e -> pure $ Left e
        Right s' -> do
          STM.writeTVar mutablePipeSpace s'
          pure . Right $ somePipeSig pipe

rootPipeSpace :: SomePipeSpace Dynamic
rootPipeSpace =
  emptySomePipeSpace "Root"
  & spsInsertScopeAt mempty rootScope

rootScope :: SomePipeScope Dynamic
rootScope =
  pipeScope ""
     -- Generators:
     --
     [ genG  "space"           TPoint' $
       STM.readTVarIO mutablePipeSpace
       <&> Right . fmap void

     , genG  "ground"          TSet' $
       -- pure . Right $ Set.fromList groundTypeNames
       pure . Right $ groundTypeNames

     , genG  "unit"            TPoint' $
       pure (Right ())

     -- Normal functions, lifted:
     --
     , linkG "strlen"  TPoint' TPoint' $
       \(str :: Text) ->
         pure . Right $ Text.length str

     -- Listing pipes and scopes:
     --
     , linkG "pipes"   TPoint' TSet' $
       \name ->
         STM.readTVarIO mutablePipeSpace
         <&> (pipeScopeAt name
              >>> (toList . (coerceName <$>) . scopeNames <$>)
              >>> maybeToEither (Error $ "No scope for name: " <> pack (show name))
              >>> \x-> x :: Fallible [Name Pipe])

     , linkG "scopes"  TPoint' TSet' $
       \name ->
         STM.readTVarIO mutablePipeSpace
         -- <&> Right . Set.fromList . childScopeQNamesAt name
         <&> Right . childPipeScopeQNamesAt name

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
         >>> join (***) (tRep . unI)

     -- Listing pipes by from/to types&reps:
     --
     , linkG "to"      TPoint' TSet' $
       \name ->
         withPipe name
         $ somePipeSig >>> sOut >>> unI >>> tRep
           >>> \toRep ->
                 atomically
                 $ STM.readTVar mutablePipeSpace
                   -- <&> Right . pipeNamesFrom (Just toRep)
                   <&> Right . toList . pipeNamesFrom (Just toRep)

     , linkG "from"    TPoint' TSet' $
       \name ->
         withPipe name
         $ somePipeSig >>> sArgs
           >>> \args ->
                 atomically $
                 if null args
                 then fallM $ "Pipe has no args: " <> pack (show name)
                 -- else Right
                 else Right . toList
                 . pipeNamesFrom (Just . tRep . unI $ head args) <$> STM.readTVar mutablePipeSpace

     , linkG "fromrep" TPoint' TSet' $
       \case
         Nothing   -> atomically $
           -- Right
           Right . toList
           . pipeNamesFrom Nothing <$> STM.readTVar mutablePipeSpace
         Just name -> atomically $
           case lookupGroundByName name of
             Nothing -> fallM $ "Unknown ground: " <> pack (show name)
             Just (tdrRep -> rep) ->
               -- Right
               Right . toList
               . pipeNamesFrom (Just rep) <$> STM.readTVar mutablePipeSpace

     , linkG "torep"   TPoint' TSet' $
       \name ->
         atomically
         $ case lookupGroundByName name of
             Nothing -> fallM $ "Unknown ground: " <> pack (show name)
             Just (tdrRep -> rep) ->
               -- Right
               Right . toList
               . pipeNamesTo rep <$> STM.readTVar mutablePipeSpace

     -- Debug:
     --
     , genG "dump"     TPoint' $ do
         putStrLn =<< unpack . showPipeSpace <$> atomically (STM.readTVar mutablePipeSpace)
         pure $ Right ()

     ]
  where
    withPipe :: QName Pipe -> (SomePipe Dynamic -> Result a) -> Result a
    withPipe name f =
      atomically (lookupPipeSTM name) >>= \case
        Nothing -> fallM $ "Missing pipe: " <> pack (show name)
        Just p -> f p

    withPipePure :: QName Pipe -> (SomePipe Dynamic -> a) -> Result a
    withPipePure name f =
      withPipe name $ pure . Right . f
