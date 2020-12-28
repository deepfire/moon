module Wire.WSS.Bearer (module Wire.WSS.Bearer) where

import Control.Exception                                 (throwIO)
import Control.Monad.Class.MonadTime                     (Time, getMonotonicTime)
import Control.Tracer

import Data.ByteString.Lazy                  qualified as LBS

import Network.Mux                           qualified as Mux
import Network.Mux.Codec                     qualified as Mux
import Network.Mux.Types                                 (MuxBearer)
import Network.Mux.Types                     qualified as Mux
import Network.Mux.Time                      qualified as Mux
import Network.Mux.Timeout                   qualified as Mux

import Network.WebSockets                    qualified as WS

-- |
-- Create @'MuxBearer'@ from a websocket 'Connection'.
--
-- Note: exceptions thrown by the WS layer are wrapped in 'MuxError'.
--
wsConnectionAsMuxBearer
  :: Tracer IO Mux.MuxTrace
  -> WS.Connection
  -> MuxBearer IO
wsConnectionAsMuxBearer tr conn =
      Mux.MuxBearer {
        Mux.read    = readMux,
        Mux.write   = writeMux,
        Mux.sduSize = 12288
      }
    where
      readMux :: Mux.TimeoutFn IO -> IO (Mux.MuxSDU, Time)
      readMux _ = do
          traceWith tr $ Mux.MuxTraceRecvHeaderStart
          buf <- WS.receiveData conn
          let (hbuf, payload) = LBS.splitAt 8 buf
          case Mux.decodeMuxSDU hbuf of
              Left  e      -> throwIO e
              Right header -> do
                  traceWith tr $ Mux.MuxTraceRecvHeaderEnd (Mux.msHeader header)
                  ts <- getMonotonicTime
                  traceWith tr $ Mux.MuxTraceRecvDeltaQObservation (Mux.msHeader header) ts
                  return (header {Mux.msBlob = payload}, ts)

      writeMux :: Mux.TimeoutFn IO -> Mux.MuxSDU -> IO Time
      writeMux _ sdu = do
          ts <- getMonotonicTime
          let ts32 = Mux.timestampMicrosecondsLow32Bits ts
              sdu' = Mux.setTimestamp sdu (Mux.RemoteClockModel ts32)
              buf  = Mux.encodeMuxSDU sdu'
          traceWith tr $ Mux.MuxTraceSendStart (Mux.msHeader sdu')
          WS.sendBinaryData conn buf
          traceWith tr $ Mux.MuxTraceSendEnd
          return ts
