module Lift.Pipe (module Lift.Pipe) where

import Data.Text qualified                     as Text

import Control.Concurrent.STM qualified        as STM
import Control.Concurrent.STM                    (TVar)
import System.IO.Unsafe qualified              as Unsafe

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Ground
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Pipe
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Scope.SomePipe
import Dom.Sig
import Dom.SomeType
import Dom.Space.Pipe
import Dom.Space.SomePipe

import Ground.Table()

import Lift.Hackage as Hackage
import Lift.Haskell as Haskell



initialPipeSpace :: SomePipeSpace Dynamic
initialPipeSpace
  =  rootPipeSpace
  <> Hackage.pipeSpace       mempty
  <> Haskell.pipeSpace       mempty

getState :: STM (SomePipeSpace Dynamic)
getState = STM.readTVar mutablePipeSpace
{-# NOINLINE getState #-}

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

mkJust :: Typeable a => a -> Maybe a
mkJust = Just

getThePipeSpace :: IO (PipeSpace (SomePipe ()))
getThePipeSpace =
  STM.readTVarIO mutablePipeSpace <&> fmap void

rootScope :: SomePipeScope Dynamic
rootScope =
  pipeScope ""
     -- Generators:
     --
     [ somePipe0 "space"  capsTSG          CVPoint
       (Right <$> getThePipeSpace)

     , somePipe0 "ground" capsTSG          CVSet $
       -- pure . Right $ Set.fromList groundTypeNames
       pure . Right $ groundTypeNames

     , somePipe0 "unit"   capsTSG          CVPoint $
       pure (Right ())

     , somePipe0 "two"    capsTSG          CVPoint $
       pure (Right (2 :: Integer))

     , somePipe2 "+"      capsTSG  CVPoint CVPoint CVPoint $
       \x y -> pure (Right (x + y :: Integer))

     , somePipe2 "*"      capsTSG  CVPoint CVPoint CVPoint $
       \x y -> pure (Right (x * y :: Integer))

     -- Normal functions, lifted:
     --
     , somePipe1 "strlen" capsTSG  CVPoint CVPoint $
       \(str :: Text) ->
         pure . Right $ Text.length str

     -- , somePipe1 "just"  CVPoint CVPoint $
     --     pure . Right . mkJust

     -- Listing pipes and scopes:
     --
     , somePipe1 "pipes"  capsTSG  CVPoint CVSet $
       \name ->
         STM.readTVarIO mutablePipeSpace
         <&> (pipeScopeAt name
              >>> (toList . (coerceName <$>) . scopeNames <$>)
              >>> maybeToEither (Error $ "No scope for name: " <> pack (show name))
              >>> \x-> x :: Fallible [Name Pipe])

     , somePipe1 "scopes" capsTSG  CVPoint CVSet $
       \name ->
         STM.readTVarIO mutablePipeSpace
         -- <&> Right . Set.fromList . childScopeQNamesAt name
         <&> Right . childPipeScopeQNamesAt name

     -- Pipe sigs:
     --
     , somePipe1 "sig"    capsTSG CVPoint CVPoint $
       \name ->
         withPipePure name
         somePipeSig

     , somePipe1 "repsig" capsTSG CVPoint CVPoint $
       \name ->
         withPipePure name
         $ somePipeSig
         >>> head . sArgs &&& sOut
         >>> join (***) (tRep . unI)

     -- Listing pipes by from/to types&reps:
     --
     , somePipe1 "to"     capsTSG CVPoint CVSet $
       \name ->
         withPipe name
         $ somePipeSig >>> sOut >>> unI >>> tRep
           >>> \toRep ->
                 atomically
                 $ STM.readTVar mutablePipeSpace
                   -- <&> Right . pipeNamesFrom (Just toRep)
                   <&> Right . toList . pipeNamesFrom (Just toRep)

     , somePipe1 "from"   capsTSG CVPoint CVSet $
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

     , somePipe1 "fromrep" capsTSG CVPoint CVSet $
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

     , somePipe1 "torep"    capsTSG CVPoint CVSet $
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
     , somePipe0 "dump"     capsTSG CVPoint $ do
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
