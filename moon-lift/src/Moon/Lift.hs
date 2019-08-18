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
  , channelFromWebsocket
  )
where

import           Codec.Serialise
import           Control.Exception
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import           Data.Map                         (Map)
import qualified Data.Set                       as Set
import           Data.Set                         (Set)
import           Data.Text
import           Data.Time
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
import           Moon.Lift.Hackage
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

data Env =
  Env
  { envConfig      :: !(Config Final)
  , envTrace       :: !(BM.Trace IO Text)
  , envSwitchboard :: !(BM.Switchboard Text)
  , envTracer      :: !(Tracer IO String)
  , envGetHackage  :: !(IO (Either Text (Set PackageName)))
  }

finalise :: Config Final -> IO Env
finalise envConfig@Config{..} = do
  (envTrace, envSwitchboard) <- BM.setupTrace_ cfMonitoring "moon"
  envGetHackage <- setupHackageCache cfHackageTmo
  let envTracer = stdoutTracer
  pure Env{..}

channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn) $
           (\(SomeException x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }

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
  . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
  => Env
  -> HaskellServer rej m a
haskellServer env@Env{..} =
    server
  where
    server = HaskellServer {
      recvMsgRequest = \req -> do
        traceWith (showTracing envTracer) req
        r <- handleRequest env req
        pure (r, server)
    , recvMsgDone = ()
    }

handleRequest :: Env -> SomeHaskellRequest -> IO (Either Text SomeHaskellReply)
handleRequest Env{..} (SomeHaskellRequest x) = case x of
  Indexes{}        -> pure . Right . PlyIndexes . RSet $
    Set.fromList [Index "hackage" "https://hackage.haskell.org/" mempty]
  Packages "hackage" -> do
    ret <- envGetHackage
    case ret of
      Left e -> pure $ Left e
      Right hackagePackages -> pure . Right . PlyPackages $ RSet hackagePackages
  Packages (IndexName ix) -> pure . Left $ "Unhandled index: " <> ix
  PackageRepo{}    -> pure . Left $ ""
  RepoPackages{}   -> pure . Left $ ""
  PackageModules{} -> pure . Left $ ""
  ModuleDeps{}     -> pure . Left $ ""
  ModuleDefs{}     -> pure . Left $ ""
  DefLoc{}         -> pure . Left $ ""

