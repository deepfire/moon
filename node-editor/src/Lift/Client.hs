{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
module Lift.Client where

import           Common.Prelude

import           Codec.Serialise                          (Serialise)
-- import           Control.Concurrent.STM                   (STM, atomically)
-- import qualified Control.Concurrent.STM                 as STM
import           Control.Concurrent.STM.TBQueue         as Q
import           Control.Exception                        (SomeException(..))
import           Control.Tracer
import qualified Data.ByteString.Lazy                   as LBS

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Channel
import qualified Network.TypedProtocol.Channel          as Net
import           Network.TypedProtocol.Codec

import qualified JavaScript.WebSockets                  as WS
import qualified JavaScript.WebSockets.FFI              as WSFFI

import qualified NodeEditor.Event.Connection            as Connection
import           NodeEditor.Event.Source                  (AddHandler(..))

import Basis
import Wire.Peer
import Wire.Protocol
import Type


data Addr = Addr
  { aHost :: String
  , aPort :: Int
  }

data Connection rej = Connection
  { cRequests :: !(Q.TBQueue SomeRequest)
  , cReplies :: !(Q.TBQueue (Either rej SomeReply))
  , cChannel :: Net.Channel IO LBS.ByteString
  , cSocket :: WS.Connection
  }

mkConnection :: WS.Connection -> Connection rej
mkConnection conn =
  Connection
  { cRequests = Q.newTBQueueIO 10
  , cReplies  = Q.newTBQueueIO 10
  , cChannel  = channel
  , cSocket   = conn
  }
 where
   channel = Net.Channel
     { recv = catch ((LBS.fromStrict <$>) <$> WS.receiveMaybe conn) $
              (\(SomeException _x) -> pure Nothing)
     , send = WS.send conn . LBS.toStrict
     }
   _tracer :: Show x => String -> x -> x
   _tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x

request
  :: (Serialise rej, Show rej, rej ~ Text)
  => Connection rej
  -> SomeRequest
  -> IO ()
request c req = atomically $ do
  Q.writeTBQueue (cRequests c) req

webSocketHandler :: WS.Connection -> AddHandler Event
webSocketHandler conn = AddHandler $ \h -> do
  let Connection{cChannel} = mkConnection conn

  void $ WSFFI.onOpen conn $
    h $ Connection Connection.Opened
  void $ WSFFI.onClose conn $ \event -> do
    code <- WSFFI.getCode event
    h $ Connection $ Connection.Closed code
  WebSocket.onError conn $
    h $ Connection Connection.Error
  void $ WS.onMessage conn $ \event -> do
    payload <- undefined
    let frame = BatchConnection.deserialize payload
    mapM_ (h . Connection . Connection.Message) $
      frame ^. BatchConnection.messages

-- runClient tracer client channel = runPeer (showTracing tracer) codec peerId channel peer
--   where
--     peerId = "server"
--     peer   = clientPeer client

runPeer
  :: forall ps (st :: ps) pr peerid failure bytes m a .
     (MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a

runPeer tr Codec{encode, decode} peerid channel@Channel{send} =
    go Nothing
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps pr st' m a
       -> m a
    go  trailing (Effect k) = k >>= go trailing
    go _trailing (Done _ x) = return x
    -- TODO: we should return the trailing here to allow for one protocols to
    -- be run after another on the same channel. It could be the case that the
    -- opening message of the next protocol arrives in the same data chunk as
    -- the final message of the previous protocol. This would also mean we
    -- would need to pass in any trailing data as an input. Alternatively we
    -- would need to move to a Channel type that can push back trailing data.

    go trailing (Yield stok msg k) = do
      traceWith tr (TraceSendMsg peerid (AnyMessage msg))
      send (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tr (TraceRecvMsg peerid (AnyMessage msg))
          go trailing' (k msg)
        Left failure ->
          throwM failure

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

connect
  :: (Serialise rej, Show rej, rej ~ Text)
  => Addr
  -> IO (Connection rej)
connect addr = mdo
  conn <- Connection
    <$> pure addr
    <*> Q.newTBQueueIO 10
    <*> Q.newTBQueueIO 10
    <*> (Async.async $ runConnection conn)
  pure conn
