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
import Pipe
import Wire.Peer
import Wire.Protocol

import qualified System.IO.Unsafe                       as Unsafe


cli :: IO ()
cli = do
  mreq <- execParser $ (info $ (optional parseRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req    = fromMaybe (Run "meta.space") mreq
          chan   = channelFromWebsocket conn
      runClient tracer (client tracer req) $ channelFromWebsocket conn

client
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ Reply)
  => Tracer m String
  -> Request
  -> m (ClientState rej m a)
client tracer req = pure $
  ClientRequesting req handleReply
 where
   handleReply (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure ClientDone
   handleReply (Right (ReplyValue rep)) = do
     putStrLn $ show rep
     case
       withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) rep
       (\(VPoint space) -> do
           traceWith tracer $ "Got pipes.."
           case (,) (lookupSpace "meta.fromrep" space)
                    (lookupSpace "meta.unit" space)
             of (,) (Just fromrep)
                    (Just unit) -> do
                  case apply (app opsDesc) fromrep
                       (SomeValue
                        (SomeKindValue
                         TPoint (VPoint (Name "Unit" :: Name Type)))) of
                    Left e -> putStrLn (unpack e)
                    Right p -> putStrLn (show p)
       ) of
       Right x -> x
       Left  e -> fail (unpack e)
     pure ClientDone
