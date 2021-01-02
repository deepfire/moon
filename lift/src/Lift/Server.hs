{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lift.Server (module Lift.Server) where

import Control.Tracer
import Control.Concurrent.Chan.Unagi    qualified as Unagi
import Control.Concurrent.STM           qualified as STM

import Data.Time

import Reflex                                hiding (Request)
import Reflex.Lift.Host

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Ground.Hask
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.SomeValue
import Dom.Space.Pipe
import Dom.Value

import Ground.Table

import Lift.Pipe



liftServer ::
  forall t m
  . MonadLiftServer t m
  => Env
  -> Unagi.InChan  StandardReply
  -> Event t StandardRequest
  -> m (Event t ())
liftServer env repsW reqsE = do
  repsE <- performEventAsync $
    ffor reqsE \sreq@(rid, req) fire -> do
      rep <- liftIO $ handleRequest env sreq
      traceM $ mconcat [ show rid, ": processed ", show req, " to: ", show rep ]
      liftIO $ fire (rid, rep)
      pure ()

  void $ performEvent $
    ffor repsE \(rid, rep) -> do
      liftIO $ Unagi.writeChan repsW (rid, rep)
      traceM $ mconcat [ show rid, ": queued reply: ", show rep ]

  pure $ never

handleRequest :: Env -> StandardRequest -> IO (Either EPipe Reply)
handleRequest Env{} req = runExceptT $ case snd req of
  Run      expr -> runRun      expr
  Let name expr -> runLet name expr

runRun :: HasCallStack => Expr (Located (QName Pipe)) -> ExceptT EPipe IO Reply
runRun expr = do
  spc <- liftIO $ atomically getState

  runnable <- newExceptT . pure $
    compile opsFull (lookupSomePipe spc) expr

  ReplyValue <$> newPipeExceptErr EExec (runSomePipe runnable)

runLet :: QName Pipe -> Expr (Located (QName Pipe)) -> ExceptT EPipe IO Reply
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

    liftIO $ ReplyValue
      . SV CPoint . SVK VPipeSpace capsTSG
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
