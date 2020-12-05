{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lift
  ( main
  , defaultConfig
  , Config(..)
  , channelFromWebsocket
  )
where

import           Codec.Serialise
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Map                         as Map
import           Data.Map                           (Map)
import qualified Data.Set.Monad                   as Set
import           Data.Set.Monad                     (Set)
import           Data.Text
import           GHC.Generics                       (Generic)
import           GHC.Types                          (Symbol)
import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty
import           Options.Applicative
import           Options.Applicative.Common
import qualified Type.Reflection                  as R

-- import qualified Control.Concurrent.Chan.Unagi    as Unagi
import qualified Control.Concurrent               as Conc
import qualified Control.Concurrent.STM           as STM
import           Control.Concurrent.STM             (STM, TVar, atomically)
import           Control.Exception           hiding (TypeError)
import           Control.Monad                      (forever)
import           Control.Tracer
import           Data.Time
import qualified Network.WebSockets               as WS
import           Shelly                             (liftIO, run, shelly)
import           System.Environment
import qualified Unsafe.Coerce                    as Unsafe

-- import qualified Cardano.BM.Configuration         as BM
-- import qualified Cardano.BM.Configuration.Model   as BM
-- import qualified Cardano.BM.Setup                 as BM
-- import qualified Cardano.BM.Backend.Switchboard   as BM
-- import qualified Cardano.BM.Trace                 as BM

import qualified Network.TypedProtocol.Channel    as Net

import           Generics.SOP.Some
import qualified Generics.SOP.Some                as SOP

import Basis

import Dom.CTag
import Dom.Error
import Dom.Expr
import Dom.Ground.Hask
import qualified Dom.Ground.Hask as Hask
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.Sig
import Dom.SomeValue
import Dom.Space.SomePipe
import Dom.Value

import Ground.Expr
import Ground.Table

import Wire.Peer
import Wire.Protocol

import Lift.Pipe

import TopHandler


import Dom.Space.Pipe



-- * Actually do things.
--
main :: IO ()
main = toplevelExceptionHandler $ do
  sealGround
  commd <- execParser opts
  let preConfig@Config{..} = defaultConfig

  libDir <- case cfGhcLibDir of
    Just x  -> pure x
    Nothing -> (FileName <$>) <$> shelly $ run "ghc" ["--print-libdir"]

  let config :: Config Final = preConfig
        { cfGhcLibDir  = libDir }

  config' <- finalise config

  case commd of
    Daemon -> wsServer config'
    Exec rq -> handleRequest config' rq >>= print

 where
   opts = info (cli <**> helper)
     ( fullDesc
    <> progDesc "A simple lift"
    <> header "lift - execute requests, either on CLI or over WebSockets" )
   cli :: Parser Cmd
   cli = subparser $ mconcat
     [ cmd "daemon" $ pure Daemon
     , cmd "exec" $ Exec <$> cliRequest
     ]
   cmd name p = command name $ info (p <**> helper) mempty

data Cmd
  = Daemon
  | Exec StandardRequest


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

data Env =
  Env
  { envConfig      :: !(Config Final)
  , envTracer      :: !(Tracer IO String)
  }

finalise :: Config Final -> IO Env
finalise envConfig@Config{..} = do
  let envTracer = stdoutTracer
  pure Env{..}

channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn)
           (\(SomeException _x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }
  where
    tracer :: Show x => String -> x -> x
    tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x

wsServer :: Env -> IO ()
wsServer env@Env{envConfig=Config{..},..} = forever $
  WS.runServer cfWSBindHost cfWSPortOut $
    \pending-> do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn cfWSPingTime

      let handleDisconnect :: WS.ConnectionException -> IO ()
          handleDisconnect x = putStrLn $ "Web disconnected: " <> show x
          tracer = stdoutTracer

      handle handleDisconnect . forever $
        runServer tracer (haskellServer env) $ channelFromWebsocket conn

haskellServer
  :: forall m a
  . (m ~ IO, a ~ Reply)
  => Env
  -> Server EPipe m a
haskellServer env@Env{..} =
    server
  where
    server = Server {
      processRequest = \req -> do
        traceWith (showTracing envTracer) req
        r <- handleRequest env req
        pure (r, server)
    , processDone = ()
    }

handleRequest :: Env -> StandardRequest -> IO (Either EPipe Reply)
handleRequest Env{..} req = runExceptT $ case req of
  Run      expr -> runRun      expr
  Let name expr -> runLet name expr

runRun :: Expr (Located (QName Pipe)) -> ExceptT EPipe IO Reply
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
      . SomeValue CPoint . SomeValueKinded VPipeSpace
      . mkValue CPoint VPipeSpace <$>
      getThePipeSpace

