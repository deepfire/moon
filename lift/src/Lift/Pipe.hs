module Lift.Pipe (module Lift.Pipe) where

import Data.Text qualified                     as Text
import Data.Vector qualified                   as Vec

import Control.Concurrent.STM qualified        as STM
import Control.Concurrent.STM                    (TVar)
import System.IO.Unsafe qualified              as Unsafe

import Reflex hiding (Dynamic)

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Ground
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Pipe
import Dom.Result
import Dom.Scope
import Dom.Scope.SomePipe
import Dom.Sig
import Dom.SomePipe
import Dom.SomeType
import Dom.Space.Pipe
import Dom.Space.SomePipe

import Ground.Table()

import Lift.Hackage as Hackage
import Lift.Haskell as Haskell



initialPipeSpace :: SomePipeSpace Dynamic
initialPipeSpace
  =  rootPipeSpace someLTagLive
  <> Hackage.pipeSpace       mempty
  <> Haskell.pipeSpace       "Hask"

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

rootPipeSpace :: SomeLTag -> SomePipeSpace Dynamic
rootPipeSpace l =
  emptySomePipeSpace "Root"
  & spsInsertScopeAt mempty (rootScope l)

mkJust :: Typeable a => a -> Maybe a
mkJust = Just

getThePipeSpace :: IO (PipeSpace (SomePipe ()))
getThePipeSpace =
  STM.readTVarIO mutablePipeSpace <&> fmap void

rootScope :: SomeLTag -> SomePipeScope Dynamic
rootScope (SomeLTag lLive@LLive{}) =
  pipeScope ""
     -- Generators:
     --
     [ somePipe0 "space" LNow capsTSG          CVPoint
       (Right <$> getThePipeSpace)

     , somePipe0 "seconds" lLive capsTSG          CVPoint $
       -- pure . Right $ Set.fromList groundTypeNames
         tickLossyFromPostBuildTime 0.1
           <&> fmap (Right . _tickInfo_n)

     , somePipe0 "ground" LNow capsTSG          CVSet $
       -- pure . Right $ Set.fromList groundTypeNames
       pure . Right $ groundTypeNames

     , somePipe0 "unit" LNow capsTSG          CVPoint $
       pure (Right ())

     , somePipe0 "two" LNow capsTSG          CVPoint $
       pure (Right (2 :: Integer))

     , somePipe2 "+" LNow capsTSG  CVPoint CVPoint CVPoint $
       \x y -> pure (Right (x + y :: Integer))

     , somePipe2 "*" LNow capsTSG  CVPoint CVPoint CVPoint $
       \x y -> pure (Right (x * y :: Integer))

     -- Normal functions, lifted:
     --
     , somePipe1 "strlen" LNow capsTSG  CVPoint CVPoint $
       \(str :: Text) ->
         pure . Right $ Text.length str

     -- , somePipe1 "just"  CVPoint CVPoint $
     --     pure . Right . mkJust

     -- Listing pipes and scopes:
     --
     , somePipe1 "pipes" LNow capsTSG  CVPoint CVSet $
       \name ->
         STM.readTVarIO mutablePipeSpace
         <&> (pipeScopeAt name
              >>> (Vec.fromList . toList . (coerceName <$>) . scopeNames <$>)
              >>> maybeToEither (Error $ "No scope for name: " <> pack (show name))
              >>> \x-> x :: Fallible (Vector (Name Pipe)))

     , somePipe1 "scopes" LNow capsTSG  CVPoint CVSet $
       \name ->
         STM.readTVarIO mutablePipeSpace
         -- <&> Right . Set.fromList . childScopeQNamesAt name
         <&> Right . Vec.fromList . childPipeScopeQNamesAt name

     -- Pipe sigs:
     --
     , somePipe1 "sig" LNow capsTSG CVPoint CVPoint $
       \name ->
         withPipePure name
         somePipeSig

     , somePipe1 "repsig" LNow  capsTSG CVPoint CVPoint $
       \name ->
         withPipePure name
         $ somePipeSig
         >>> head . sArgs &&& sOut
         >>> join (***) (tRep . unI)

     -- Listing pipes by from/to types&reps:
     --
     , somePipe1 "to" LNow capsTSG CVPoint CVSet $
       \name ->
         withPipe name
         $ somePipeSig >>> sOut >>> unI >>> tRep
           >>> \toRep ->
                 atomically
                 $ STM.readTVar mutablePipeSpace
                   -- <&> Right . pipeNamesFrom (Just toRep)
                   <&> Right . Vec.fromList . toList . pipeNamesFrom (Just toRep)

     , somePipe1 "from" LNow capsTSG CVPoint CVSet $
       \name ->
         withPipe name
         $ somePipeSig >>> sArgs
           >>> \args ->
                 atomically $
                 if null args
                 then fallM $ "Pipe has no args: " <> pack (show name)
                 -- else Right
                 else Right . Vec.fromList . toList
                 . pipeNamesFrom (Just . tRep . unI $ head args) <$> STM.readTVar mutablePipeSpace

     , somePipe1 "fromrep" LNow capsTSG CVPoint CVSet $
       \case
         Nothing   -> atomically $
           -- Right
           Right . Vec.fromList . toList
           . pipeNamesFrom Nothing <$> STM.readTVar mutablePipeSpace
         Just name -> atomically $
           case lookupGroundByName name of
             Nothing -> fallM $ "Unknown ground: " <> pack (show name)
             Just (tdrRep -> rep) ->
               -- Right
               Right . Vec.fromList . toList
               . pipeNamesFrom (Just rep) <$> STM.readTVar mutablePipeSpace

     , somePipe1 "torep" LNow capsTSG CVPoint CVSet $
       \name ->
         atomically
         $ case lookupGroundByName name of
             Nothing -> fallM $ "Unknown ground: " <> pack (show name)
             Just (tdrRep -> rep) ->
               -- Right
               Right . Vec.fromList . toList
               . pipeNamesTo rep <$> STM.readTVar mutablePipeSpace

     -- Debug:
     --
     , somePipe0 "dump" LNow capsTSG CVPoint $ do
         putStrLn =<< unpack . showPipeSpace <$> atomically (STM.readTVar mutablePipeSpace)
         pure $ Right ()

     ]
  where
    withPipe :: QName Pipe -> (SomePipe Dynamic -> Result Now a) -> Result Now a
    withPipe name f =
      atomically (lookupPipeSTM name) >>= \case
        Nothing -> fallM $ "Missing pipe: " <> pack (show name)
        Just p -> f p

    withPipePure :: QName Pipe -> (SomePipe Dynamic -> a) -> Result Now a
    withPipePure name f =
      withPipe name $ pure . Right . f
