{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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

main :: IO ()
main = do
  mreq <- execParser $ (info $ (optional parseHaskellRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req = fromMaybe (SomeHaskellRequest Indexes) mreq
      runClient tracer (haskellClient tracer req) $ channelFromWebsocket conn

haskellClient
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
  => Tracer m String
  -> SomeHaskellRequest
  -> HaskellClient rej m a
haskellClient tracer req = SendMsgRequest req handleReply
  where
    handleReply (Left rej) = do
      putStrLn $ "error: " <> unpack rej
      pure SendMsgDone
    handleReply (Right rep) = do
      putStrLn $ show rep
      pure SendMsgDone
