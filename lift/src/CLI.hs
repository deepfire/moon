module CLI (cli) where

import           Codec.Serialise                          (Serialise)
import           Control.Tracer
                 (Tracer, stdoutTracer, showTracing, traceWith)
import qualified Data.ByteString.Lazy                   as LBS
import           Data.Maybe
import           Data.Text
import qualified Network.WebSockets                     as WS
import           Options.Applicative
import           Options.Applicative.Common

import           Control.Concurrent
-- import qualified Control.Concurrent.STM.TMVar           as TM
-- import qualified Control.Concurrent.STM.TBQueue         as TQ

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Core             as Net
import qualified Network.TypedProtocol.Driver           as Net

import Ground.Hask
import Lift
import Wire.Peer
import Wire.Protocol

import qualified System.IO.Unsafe                 as Unsafe


-- peerQ
--   :: (rej ~ Text, m ~ IO)
--   => MVar (CState rej m)
-- peerQ = Unsafe.unsafePerformIO $ newEmptyMVar
-- {-# NOINLINE peerQ #-}

cli :: IO ()
cli = do
  mreq <- execParser $ (info $ (optional parseSomeRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req = fromMaybe (SomeRequest $ Run "meta.descs") mreq
          chan = channelFromWebsocket conn
      runClient tracer (client tracer req) $ channelFromWebsocket conn
      -- st <- runAsyncPeer (showTracing tracer) codec "server" chan Nothing $
      --   mkAsyncSubmitPeer (ClientAgency TokIdle) $ mkAsyncRequest req
      -- runAsyncPeer (showTracing tracer) codec "server" chan Nothing $
      --   resumeAsyncPeer (ServerAgency TokBusy) st
      -- pure ()

client
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ SomeReply)
  => Tracer m String
  -> SomeRequest
  -> m (ClientState rej m a)
client tracer req = pure $
  ClientRequesting req handleReply
 where
   handleReply (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure ClientDone
   handleReply (Right rep) = do
     putStrLn $ show rep
     pure ClientDone
