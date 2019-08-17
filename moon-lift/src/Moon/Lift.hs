{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Moon.Lift
  ( main
  , defaultConfig
  , Config(..)
  )
where

import           Codec.Serialise
import           Control.Exception
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text
import           GHC.Generics                     (Generic)
import           Options.Applicative

import qualified Control.Concurrent.Chan.Unagi  as Unagi
import qualified Control.Concurrent             as Conc
import           Control.Exception
import           Control.Monad                    (forever)
import           Control.Tracer
import qualified Network.WebSockets             as WS
import           Shelly
import           System.Environment

import qualified Cardano.BM.Configuration       as BM
import qualified Cardano.BM.Configuration.Model as BM
import qualified Cardano.BM.Setup               as BM
import qualified Cardano.BM.Backend.Switchboard as BM
import qualified Cardano.BM.Trace               as BM

import qualified Network.TypedProtocol.Channel  as Net

import           Moon.Face
import           Moon.Face.Haskell
import           Moon.Protocol
import           Moon.Peer
import           Moon.Lift.Haskell


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
  }

data RuntimeConfig =
  RuntimeConfig
  { rtConfig      :: Config Final
  , rtTrace       :: BM.Trace IO Text
  , rtSwitchboard :: BM.Switchboard Text
  }

finalise :: Config Final -> IO RuntimeConfig
finalise rtConfig@Config{..} = do
  (rtTrace, rtSwitchboard) <- BM.setupTrace_ cfMonitoring "moon"
  pure RuntimeConfig{..}

main :: IO ()
main = do
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

wsServer :: RuntimeConfig -> IO ()
wsServer RuntimeConfig{rtConfig=Config{..},..} = forever $ do
  -- (,) fromWebW fromWebR <- Unagi.newChan

  WS.runServer cfWSBindHost cfWSPortOut $
    \pending-> do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn cfWSPingTime

      let handleDisconnect :: WS.ConnectionException -> IO ()
          handleDisconnect x = putStrLn $ "Web disconnected: " <> show x
          tracer = stdoutTracer

      flip catch handleDisconnect $ forever $ do
        runServer tracer (haskellServer tracer) $ channelFromWebsocket conn

haskellServer
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
  => Tracer m String
  -> HaskellServer rej m a
haskellServer tracer =
    server
  where
    server = HaskellServer {
      recvMsgRequest = \req -> do
        traceWith (showTracing tracer) req
        r <- handleRequest req
        pure (r, server)
    , recvMsgDone = ()
    }

handleRequest :: SomeHaskellRequest -> IO (Either Text SomeHaskellReply)
handleRequest (SomeHaskellRequest x) = case x of
  Indexes{}        -> pure . Right $ PlyIndexes $ RList [Index "hackage" "https://hackage.haskell.org/" mempty]
  PackageRepo{}    -> pure . Left $ ""
  RepoPackages{}   -> pure . Left $ ""
  PackageModules{} -> pure . Left $ ""
  ModuleDeps{}     -> pure . Left $ ""
  ModuleDefs{}     -> pure . Left $ ""
  DefLoc{}         -> pure . Left $ ""
