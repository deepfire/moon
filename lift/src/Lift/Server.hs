{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lift.Server (module Lift.Server) where

import Control.Tracer
import Control.Concurrent.Chan.Unagi    qualified as Unagi
import Control.Concurrent.STM           qualified as STM
import Control.Monad                                (forever)

import Data.Time
import Data.IntUnique

import Reflex                                hiding (Request)
import Reflex.Lift.Host

import Unsafe.Coerce                    qualified as Unsafe

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Ground.Hask
import Dom.Located
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Reflex
import Dom.RequestReply
import Dom.Result
import Dom.SomePipe
import Dom.SomeValue
import Dom.Space.Pipe
import Dom.Value

import Ground.Table

import Lift.Pipe



liftRequestLoop ::
     Env
  -> Unagi.OutChan StandardRequest
  -> Unagi.InChan  StandardReply
  -> IO ()
liftRequestLoop env reqsR repsW = forever $ do
  sreq@(rid, _) <- Unagi.readChan reqsR

  res <- handleRequest env sreq
  case res of
    Left e -> Unagi.writeChan repsW (rid, Left e)
    Right (SR LNow ioa) ->
      ((rid,) . mapLeft EExec <$> ioa)
      >>= Unagi.writeChan repsW
      >> putStrLn ("processed request: " <> show sreq)
    Right (SR LLive{} network) ->
      runLiftEventServer $
        Unsafe.unsafeCoerce $
          liftEventServer rid repsW $
            network

liftEventServer ::
  forall t m
  . MonadReflex t m
  => Unique
  -> Unagi.InChan StandardReply
  -> m (Event t (Fallible SomeValue))
  -> m (Event t ())
liftEventServer rid repsW network = do
  repsE :: Event t (PFallible SomeValue) <-
    fmap (mapLeft EExec) <$> network

  void $ performEvent $
    ffor repsE \rep -> do
      traceM $ mconcat [ show rid, ": queuing reply: ", show rep ]
      liftIO $ Unagi.writeChan repsW (rid, rep)

  pure never
  --     \case
  --       Left e -> putStrLn . unpack $ "runtime error: " <> showError e
  --       Right _r -> pure ()
  -- ReplyValue <$> newPipeExceptErr EExec (runSomePipe runnable)

  -- repsE <- performEventAsync $
  --   ffor reqsE \sreq@(rid, req) fire -> do
  --     rep <- liftIO $ handleRequest env sreq
  --     traceM $ mconcat [ show rid, ": processed ", show req, " to: ", show rep ]
  --     liftIO $ fire (rid, rep)
  --     pure ()

  -- void $ performEvent $
  --   ffor repsE \(rid, rep) -> do
  --     liftIO $ Unagi.writeChan repsW (rid, rep)
  --     traceM $ mconcat [ show rid, ": queued reply: ", show rep ]

  -- pure $ never

-- TODO:  make liftRequestloop use this
mapSomeResult ::
     (PFallible SomeValue -> IO ())
  -> SomeResult
  -> IO ()
mapSomeResult f = \case
  SR LNow    ioa -> fmap (mapLeft EExec) ioa >>= f
  SR LLive{} net ->
    runLiftEventServer $
    Unsafe.unsafeCoerce $
    reflexHandler net
 where
   reflexHandler ::
     forall t m
     . MonadReflex t m
     => m (Event t (Fallible SomeValue))
     -> m (Event t ())
   reflexHandler net = do
     repsE <-
       fmap (mapLeft EExec) <$> net
     void $ performEvent $
       ffor repsE (liftIO . f)
     pure never

setupLLive :: IO ()
setupLLive = do
  runLiftEventServer collectLLive
 where
   collectLLive ::
     forall (t :: *) (m :: * -> *)
     . (MonadReflex t m, Typeable t, Typeable m)
     => m (Event t ())
   collectLLive = do
     pb :: Event t () <- getPostBuild
     performEvent $
       ffor pb $
         (const . liftIO $
           setupSomeLTagLive (SomeLTag $ LLive (typeRep @t) (typeRep @m)))

handleRequest :: Env -> StandardRequest -> IO (Either EPipe SomeResult)
handleRequest Env{} req = runExceptT $ case snd req of
  Run      expr -> runRun      expr
  -- Let name expr -> runLet name expr

runRun :: HasCallStack => Expr (Located (QName Pipe)) -> ExceptT EPipe IO SomeResult
runRun expr = do
  spc <- liftIO $ atomically getState

  runnable <- newExceptT . pure $
    compile opsFull (lookupSomePipe spc) expr

  pure $ runSomePipe runnable

runLet :: QName Pipe -> Expr (Located (QName Pipe)) -> ExceptT EPipe IO SomeValue
runLet name expr = do
    liftIO $ putStrLn $ unpack $ mconcat
      ["let ", showQName name, " = ", pack $ show (locVal <$> expr) ]

    spc <- liftIO $ atomically getState

    void . newPipeExceptErr EName . pure $
      maybeLeft (lookupSomePipe spc
                 >>> ($> Error ("Already exists: " <> showQName name)))
                name

    pipe <- newExceptT . pure $
      compile opsFull (lookupSomePipe spc) expr

    void . newExceptT . fmap (left (EName . Error)) . liftIO . atomically $
      addPipe name pipe

    liftIO $
      putStrLn =<< unpack . showPipeSpace <$> atomically (STM.readTVar mutablePipeSpace)

    liftIO $
      SV CPoint . SVK VPipeSpace capsTSG
      . mkValue CPoint VPipeSpace <$>
      getThePipeSpace


data Env =
  Env
  { envConfig      :: !(Config Final)
  , envTracer      :: !(Tracer IO String)
  }

data ConfigPhase
  = Initial
  | Final

data Config a =
  Config
  { cfWSBindHost :: String
  , cfWSPortIn   :: Int
  , cfWSPortOut  :: Int
  , cfWSPingTime :: Int
  , cfGhcLibDir  :: CFGhcLibDir a
  , cfGitRoot    :: FileName
  , cfHackageTmo :: NominalDiffTime
  }
type family CFGhcLibDir (a :: ConfigPhase) where
  CFGhcLibDir Initial  = Maybe FileName
  CFGhcLibDir Final    = FileName

defaultConfig :: Config Initial
defaultConfig = Config
  { cfWSBindHost = "127.0.0.1"
  , cfWSPortIn   = 29670
  , cfWSPortOut  = 29671
  , cfWSPingTime = 30
  , cfGhcLibDir  = Nothing
  , cfGitRoot    = "."
  , cfHackageTmo = 3600
  }
