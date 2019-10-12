{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Lift
  ( lift
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
import           Data.Time
import           GHC.Generics                       (Generic)
import           GHC.Types                          (Symbol)
import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty
import           Options.Applicative
import qualified Type.Reflection                  as R

import qualified Control.Concurrent.Chan.Unagi    as Unagi
import qualified Control.Concurrent               as Conc
import qualified Control.Concurrent.STM           as STM
import           Control.Concurrent.STM             (STM, TVar, atomically)
import           Control.Exception           hiding (TypeError)
import           Control.Monad                      (forever)
import           Control.Tracer
import           Data.Time
import qualified Network.WebSockets               as WS
import           Shelly                             (run, shelly)
import           System.Environment
import qualified Unsafe.Coerce                    as Unsafe

import qualified Cardano.BM.Configuration         as BM
import qualified Cardano.BM.Configuration.Model   as BM
import qualified Cardano.BM.Setup                 as BM
import qualified Cardano.BM.Backend.Switchboard   as BM
import qualified Cardano.BM.Trace                 as BM

import qualified Network.TypedProtocol.Channel    as Net

import           Generics.SOP.Some
import qualified Generics.SOP.Some                as SOP


import Basis
import Ground
import Ground.Hask
import qualified Ground.Hask as Hask
import Lift.Pipe
import Pipe
import Wire.Peer
import Wire.Protocol


data ConfigPhase
  = Initial
  | Final

data Config a =
  Config
  { cfWSBindHost :: String
  , cfWSPortIn   :: Int
  , cfWSPortOut  :: Int
  , cfWSPingTime :: Int
  , cfMonitoring :: CFMonitoring a
  , cfGhcLibDir  :: CFGhcLibDir a
  , cfGitRoot    :: FileName
  , cfHackageTmo :: NominalDiffTime
  }
type family CFMonitoring (a :: ConfigPhase) where
  CFMonitoring Initial = IO BM.Configuration
  CFMonitoring Final   = BM.Configuration
type family CFGhcLibDir (a :: ConfigPhase) where
  CFGhcLibDir Initial  = Maybe FileName
  CFGhcLibDir Final    = FileName

defaultConfig :: Config Initial
defaultConfig = Config
  { cfWSBindHost = "127.0.0.1"
  , cfWSPortIn   = 29670
  , cfWSPortOut  = 29671
  , cfWSPingTime = 30
  , cfMonitoring = BM.empty
  , cfGhcLibDir  = Nothing
  , cfGitRoot    = "."
  , cfHackageTmo = fromInteger 3600
  }

lift :: IO ()
lift = do
  -- establish config
  let preConfig@Config{..} = defaultConfig
  monitoring <- cfMonitoring
  libDir <- case cfGhcLibDir of
    Just x  -> pure x
    Nothing -> (FileName <$>) <$> shelly $ run "ghc" ["--print-libdir"]
  let config :: Config Final = preConfig
        { cfMonitoring = monitoring
        , cfGhcLibDir  = libDir }

  wsServer =<< finalise config

data Env =
  Env
  { envConfig      :: !(Config Final)
  , envTrace       :: !(BM.Trace IO Text)
  , envSwitchboard :: !(BM.Switchboard Text)
  , envTracer      :: !(Tracer IO String)
  }

finalise :: Config Final -> IO Env
finalise envConfig@Config{..} = do
  (envTrace, envSwitchboard) <- BM.setupTrace_ cfMonitoring "lift"
  let envTracer = stdoutTracer
  pure Env{..}

channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn) $
           (\(SomeException _x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }
  where
    tracer :: Show x => String -> x -> x
    tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x

wsServer :: Env -> IO ()
wsServer env@Env{envConfig=Config{..},..} = forever $ do
  -- (,) fromWebW fromWebR <- Unagi.newChan

  WS.runServer cfWSBindHost cfWSPortOut $
    \pending-> do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn cfWSPingTime

      let handleDisconnect :: WS.ConnectionException -> IO ()
          handleDisconnect x = putStrLn $ "Web disconnected: " <> show x
          tracer = stdoutTracer

      flip catch handleDisconnect $ forever $ do
        runServer tracer (haskellServer env) $ channelFromWebsocket conn

haskellServer
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ SomeReply)
  => Env
  -> Server rej m a
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

handleRequest :: Env -> SomeRequest -> IO (Either Text SomeReply)
handleRequest Env{..} (SomeRequest x) = case x of
  Compile name text ->
    Lift.compile name text
    <&> (SomeReply . ReplyValue . SomeValue . SomeKindValue . VPoint <$>)
  Run text ->
    case Pipe.parse text of
      Left e -> pure . Left $ "Parse: " <> e
      Right nameTree -> do
        putStrLn $ unpack $ Data.Text.unlines
          ["Pipe:", pack $ show nameTree ]
        res <- atomically $ Pipe.compile lookupPipeFail nameTree
        case res of
          Left e -> pure . Left $ "Compilation: " <> e
          Right runnable -> do
            res :: Either Text SomeValue <- runPipe runnable
            case res of
              Left e -> pure . Left $ "Runtime: " <> e
              Right x -> pure . Right . SomeReply . ReplyValue $ x
  -- x -> pure . Left . pack $ "Unhandled request: " <> show x

compile :: QName SomePipe -> Text -> Result Sig
compile newname text =
  case Pipe.parse text of
    Left e -> pure . Left $ "Parse: " <> e
    Right nameTree -> do
      putStrLn $ unpack $ Data.Text.unlines
        ["Pipe:", pack $ show nameTree ]
      atomically $ do
        old <- lookupPipe newname
        res <- Pipe.compile lookupPipeFail nameTree
        case (old, res) of
          (Just _,  _)    -> pure . Left $ "Already exists: " <> pack (show newname)
          (_,  Left e)    -> pure . Left $ e
          (_, Right pipe) -> do
            addPipe newname pipe
            pure . Right . somePipeSig $ pipe

lookupPipeFail :: e ~ Text => QName SomePipe -> STM (Either e SomePipe)
lookupPipeFail name =
  lookupPipe name <&>
  guard ("No such pipe: " <> showQName name)
