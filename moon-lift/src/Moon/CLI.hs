{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Moon.CLI (main) where

import           Codec.Serialise
import           Control.Tracer
import           Data.Binary
import qualified Data.ByteString.Lazy           as LBS
import           Data.Maybe
import           Data.Text
import qualified Network.WebSockets             as WS
import           Options.Applicative
import           Options.Applicative.Common

import Moon.Face
import Moon.Face.Haskell
import Moon.Lift hiding (main)
import Moon.Peer
import Moon.Protocol

main :: IO ()
main = do
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
  -> Client rej m a
client tracer req = SendMsgRequest req handleReply
  where
    handleReply (Left rej) = do
      putStrLn $ "error: " <> unpack rej
      pure SendMsgDone
    handleReply (Right rep) = do
      putStrLn $ show rep
      pure SendMsgDone
