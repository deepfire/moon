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

module Moon.Lift (main) where

import qualified Data.Binary                    as Binary
import           Data.Binary                      (Binary)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text
import           GHC.Generics                     (Generic)
-- import           Options.Applicative

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
  { cfWSBindHost = ""
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
instance Binary WSMesg

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
        case Binary.decodeOrFail fromCli of
          Left  (_, _, e)   -> throw . WS.ParseException $ show e
          Right (_, _, End) -> throw $ WS.ConnectionClosed
          Right (_, _, BS bs) -> do
            resp <- handleRequestRaw bs
            case resp of
              Left e  -> throw $ WS.ParseException $ show e
              Right r -> WS.sendBinaryData conn r
      -- IO loop no more.
      WS.sendClose conn . Binary.encode $ End


handleRequestRaw :: BS.ByteString -> IO (Either Text BS.ByteString)
handleRequestRaw raw =
  case Binary.decodeOrFail $ LBS.fromStrict raw of
    Left  (_, _, e)  -> throwIO $ WS.ParseException e
    Right (_, _, rr) -> do
      r <- handleRequest rr
      pure $ case r of
        Sorry e -> Left  e
        x       -> Right . LBS.toStrict . Binary.encode $ x

-- XXX: coupling with Moon.Face
handleRequest :: RequestHaskell -> IO (Reply a)
handleRequest Indexes{}        = pure . Sorry $ ""
handleRequest RepoURL{}        = pure . Sorry $ ""
handleRequest RepoPackages{}   = pure . Sorry $ ""
handleRequest PackageModules{} = pure . Sorry $ ""
handleRequest ModuleDeps{}     = pure . Sorry $ ""
handleRequest ModuleDefs{}     = pure . Sorry $ ""
handleRequest DefLoc{}         = pure . Sorry $ ""
