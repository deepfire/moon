{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}

module CLI (cli) where

import           Codec.Serialise
import           Control.Tracer
import           Data.Binary
import qualified Data.ByteString.Lazy           as LBS
import           Data.Maybe
import           Data.Text
import qualified Network.WebSockets             as WS
import           Options.Applicative
import           Options.Applicative.Common

import Basis
import Ground.Hask
import Lift
import Wire.Peer
import Wire.Protocol

cli :: IO ()
cli = do
  mreq <- execParser $ (info $ (optional parseSomeRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req = fromMaybe (SomeRequest $ RunPipe "pipes" []) mreq
      runClient tracer (client tracer req) $ channelFromWebsocket conn

client
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ SomeReply)
  => Tracer m String
  -> SomeRequest
  -> m (Client rej m a)
client tracer req = pure $ SendMsgRequest req handleReply
  where
    handleReply (Left rej) = do
      putStrLn $ "error: " <> unpack rej
      pure SendMsgDone
    handleReply (Right rep) = do
      putStrLn $ show rep
      pure SendMsgDone
