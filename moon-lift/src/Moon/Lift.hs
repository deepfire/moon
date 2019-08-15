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
  , WSMesg(..)
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

data Runtime =
  Runtime
  { rtConfig      :: Config Final
  , rtTrace       :: BM.Trace IO Text
  , rtSwitchboard :: BM.Switchboard Text
  }

initialise :: Config Final -> IO Runtime
initialise rtConfig@Config{..} = do
  (rtTrace, rtSwitchboard) <- BM.setupTrace_ cfMonitoring "moon"
  pure Runtime{..}

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

  -- initialise runtime
  rt <- initialise config

  runServer rt

data WSMesg where
  BS  :: BS.ByteString -> WSMesg
  End :: WSMesg
  deriving (Generic)
instance Serialise WSMesg

channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn) $
           (\(SomeException x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }

runServer :: Runtime -> IO ()
runServer Runtime{rtConfig=Config{..},..} = forever $ do
  -- (,) fromWebW fromWebR <- Unagi.newChan

  -- Conc.forkIO $ WS.runServer cfWSBindHost cfWSPortIn $ fromWebForwarder cfWSPingTime fromWebW
  WS.runServer cfWSBindHost cfWSPortOut $
    -- server cfWSPingTime
    \pending-> do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn cfWSPingTime

      let handleDisconnect :: WS.ConnectionException -> IO ()
          handleDisconnect x = putStrLn $ "Web disconnected: " <> show x

      flip catch handleDisconnect $ forever $ do
        fromCli <- WS.receiveData conn
        case deserialiseOrFail fromCli of
          Left  e       -> throw . WS.ParseException $ show e
          Right (End)   -> throw $ WS.ConnectionClosed
          Right (BS bs) -> do
            resp <- handleRequestRaw bs
            case resp of
              Left e    -> throw $ WS.ParseException $ show e
              Right r   -> do
                WS.sendBinaryData conn (serialise $ BS r)
                WS.sendBinaryData conn (serialise $ End)
      -- IO loop no more.
      WS.sendClose conn . serialise $ End


handleRequestRaw :: BS.ByteString -> IO (Either Text BS.ByteString)
handleRequestRaw raw =
  case deserialiseOrFail $ LBS.fromStrict raw :: Either DeserialiseFailure SomeHaskellRequest of
    Left  e  -> throwIO $ WS.ParseException (show e)
    Right rr -> do
      r <- handleRequest rr
      pure $ case r of
        Left e  -> Left  e
        Right x -> Right . LBS.toStrict . serialise $ x

-- XXX: coupling with Moon.Face
handleRequest :: SomeHaskellRequest -> IO (Either Text HaskellReply)
handleRequest (SomeHaskellRequest x) = case x of
  Indexes{}        -> pure . Right $ PlyIndexes $ RList [Index "hackage" "https://hackage.haskell.org/" mempty]
  PackageRepo{}    -> pure . Left $ ""
  RepoPackages{}   -> pure . Left $ ""
  PackageModules{} -> pure . Left $ ""
  ModuleDeps{}     -> pure . Left $ ""
  ModuleDefs{}     -> pure . Left $ ""
  DefLoc{}         -> pure . Left $ ""
