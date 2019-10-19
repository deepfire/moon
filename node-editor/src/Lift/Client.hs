{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Lift.Client
  ( Connection
  , URL(..)
  , mkConnection
  , runConnectionWorkerIO
  , request
  )
where

import           Prelude                                  (error)
import           Common.Prelude

import           Codec.Serialise                          (Serialise)
import           Control.Concurrent.STM                   (atomically)
import           Control.Concurrent.Async               as Async
import           Control.Concurrent.STM.TBQueue         as Q
import           Control.Tracer
import qualified Data.ByteString                        as SBS
import qualified Data.ByteString.Lazy                   as LBS
import qualified GHCJS.Buffer                           as Buffer
import           GHCJS.DOM.Types                          (ArrayBuffer, toJSString)

import qualified Unsafe.Coerce                          as Unsafe

import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Core             as Net
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Channel
import qualified Network.TypedProtocol.Channel          as Net

import qualified JavaScript.Web.CloseEvent              as WS
import qualified JavaScript.Web.MessageEvent            as WS
import qualified JavaScript.Web.WebSocket               as WS

import Basis
import Wire.Peer ()
import Wire.Protocol

newtype URL = URL Text

data Connection rej f = Connection
  { _cRaw :: !(Q.TBQueue SBS.ByteString)
  , cRequests :: !(Q.TBQueue (SomeRequest, SomeReply -> f SomeReply))
  , cReplyHandler :: !(Either rej (f SomeReply) -> IO ())
  , cChannel :: !(Net.Channel IO LBS.ByteString)
  , _cSocket :: !(WS.WebSocket)
  , cTracer :: !(Tracer IO String)
  }

data ClientState rej m a
  = WaitingForRequest
  | Closing

mkClientSTS
  :: forall rej m a f
  . (m ~ IO, a ~ SomeReply)
  => Connection rej f
  -> Peer (Piping rej) AsClient StIdle m ()
mkClientSTS Connection{cRequests, cReplyHandler} =
    go WaitingForRequest
  where
    go :: ClientState rej m a
       -> Peer (Piping rej) AsClient StIdle m ()
    go WaitingForRequest =
      Effect $ do
        (,) req f <- atomically $ Q.readTBQueue cRequests
        putStrLn "Got request!"
        pure $
          Yield (ClientAgency TokIdle) (MsgRequest req) $
            Await (ServerAgency TokBusy) $ \case
              MsgReply rep ->
                Effect $ do
                  putStrLn "MsgReply"
                  Async.async (cReplyHandler (Right $ f rep))
                  pure $ go WaitingForRequest
              MsgBadRequest rej ->
                Effect $ do
                  putStrLn "MsgBadRequest"
                  Async.async (cReplyHandler (Left rej))
                  pure $ go WaitingForRequest
      -- where
      --   projectReply :: Message (Piping rej) StBusy st -> Either rej SomeReply
      --   projectReply (MsgReply rep)      = Right rep
      --   projectReply (MsgBadRequest rej) = Left  rej

    go Closing =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Net.Done TokDone ())

request
  :: (Serialise rej, Show rej, rej ~ Text)
  => Connection rej f
  -> (SomeReply -> f SomeReply)
  -> SomeRequest
  -> IO ()
request c f req = atomically $
  Q.writeTBQueue (cRequests c) (req, f)

runConnectionWorkerIO
  :: (Serialise rej, Show rej, rej ~ Text)
  => Connection rej f -> IO ()
runConnectionWorkerIO c@Connection{cChannel, cTracer} = do
  runPeer (showTracing cTracer) wireCodec "server" cChannel peer
 where
   peer = mkClientSTS c

mkConnection
  :: (Serialise rej, Show rej, rej ~ Text)
  => URL
  -> (Either rej (f SomeReply) -> IO ())
  -> IO (Connection rej f)
mkConnection (URL url) handler = do
  rawReadQueue <- Q.newTBQueueIO 10
  ws <- WS.connect (wsc rawReadQueue)
  WS.setBinaryType WS.ArrayBuffer ws
  Connection
    <$> pure rawReadQueue
    <*> Q.newTBQueueIO 10
    <*> pure handler
    <*> pure (channel rawReadQueue ws)
    <*> pure ws
    <*> pure stdoutTracer
 where
   wsc readQueue = WS.WebSocketRequest
     { WS.url = toJSString url
     , WS.protocols = []
     , WS.onMessage = Just (onMessage readQueue)
     , WS.onClose = Just onClose
     }
   onMessage :: Q.TBQueue SBS.ByteString -> WS.MessageEvent -> IO ()
   onMessage readQueue = WS.getData >>> \case
     WS.ArrayBufferData x -> atomically $ Q.writeTBQueue readQueue $
       Buffer.toByteString 0 Nothing (Buffer.createFromArrayBuffer x)
     WS.StringData{} -> error $ "Unexpected string from WS with " <> unpack url
     WS.BlobData{}   -> error $ "Unexpected blob from WS with "   <> unpack url
   onClose :: WS.CloseEvent -> IO ()
   onClose _msg = error $ "Unhandled close in WS with " <> unpack url
   channel
     :: Q.TBQueue SBS.ByteString
     -> WS.WebSocket
     -> Net.Channel IO LBS.ByteString
   channel readQueue ws = Net.Channel
     { recv = do
         r <- Just . LBS.fromStrict <$> atomically (Q.readTBQueue readQueue)
         putStrLn "recv() got data"
         pure r
     , send = flip WS.sendArrayBuffer ws
            . Unsafe.unsafeCoerce
            . byteStringToArrayBuffer
            . LBS.toStrict
     }
   byteStringToArrayBuffer :: SBS.ByteString -> ArrayBuffer
   byteStringToArrayBuffer bs =
     let (js_getArrayBuffer -> buf, start, end) = Buffer.fromByteString bs
     in js_sliceBuffer start end buf
   _tracer :: Show x => String -> x -> x
   _tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x

foreign import javascript safe "$3.slice($1, $2)"
  js_sliceBuffer :: Int -> Int -> ArrayBuffer -> ArrayBuffer

foreign import javascript unsafe
  "$1.buf" js_getArrayBuffer    :: Buffer.Buffer -> ArrayBuffer

-- webSocketHandler :: WS.Connection -> AddHandler Event
-- webSocketHandler conn = AddHandler $ \h -> do
--   let Connection{cChannel} = mkConnection conn

  -- void $ WSFFI.onClose conn $ \event -> do
  --   code <- WSFFI.getCode event
  --   h $ Connection $ Connection.Closed code
  -- WebSocket.onError conn $
  --   h $ Connection Connection.Error
  -- void $ WS.onMessage conn $ \event -> do

  --   runAsyncPeer (showTracing tracer) codec "server" chan Nothing $
  --     resumeAsyncPeer (ServerAgency TokBusy) st
  --   payload <- undefined
  --   let frame = BatchConnection.deserialize payload
  --   mapM_ (h . Connection . Connection.Message) $
  --     frame ^. BatchConnection.messages
