{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-missing-home-modules #-}
module Main where

import           Prelude                    (error)
import           Common.Prelude
import           Common.ClientId            (clientId)
import           Control.Concurrent.Chan    (Chan)
import qualified Control.Concurrent.Chan    as Chan
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified JS.Mount                   as Mount
import           NodeEditor.Event.Engine    (LoopRef (LoopRef))
import qualified NodeEditor.Event.Event                 as Event
import qualified NodeEditor.Event.Engine    as Engine
import qualified NodeEditor.React.Model.App as App
import qualified NodeEditor.React.Store     as Store
import qualified NodeEditor.React.View.App  as App
import           NodeEditor.State.Global    (mkState)
import qualified React.Flux                 as React
import           System.Random              (newStdGen)
import           GHC.Stack                  (HasCallStack)

import Debug.Trace (trace)
import Text.Printf (printf)
import           Control.Tracer
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Text                        as Text
import qualified JavaScript.WebSockets            as WS
import qualified Network.TypedProtocol.Channel    as Net
import Wire.Peer
import Wire.Protocol

runApp :: Chan (IO ()) -> WS.Connection -> IO ()
runApp chan socket = do
    random         <- newStdGen
    mdo
        let loop = LoopRef chan state
            evScheduler = Engine.scheduleEvent loop
        evScheduler Event.Init
        appRef <- Store.createApp (App.mk Nothing) evScheduler
        React.reactRender Mount.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId random
        state <- newMVar initState
        pure ()
        -- Engine.connectEventSources socket loop
    App.focus

withActiveConnection :: HasCallStack => (WS.Connection -> IO ()) -> IO ()
withActiveConnection action = do
  WS.withUrl "ws://127.0.0.1:29671" $ \conn -> do
    let tracer = stdoutTracer
        req = (SomeRequest $ Run "pipes")
    runClient tracer (client tracer req) $ channelFromWebsocket' conn
    -- WS.sendMessage conn (WS.SocketMsgText "LOL")
    -- action conn

channelFromWebsocket' :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket' conn =
  Net.Channel
  { recv = catch ((-- tracer "recv" .
                   LBS.fromStrict <$>)
                  <$> WS.receiveMaybe conn) $
           (\(SomeException _x) -> pure Nothing)
  , send = WS.send conn
    -- . tracer "send"
    . LBS.toStrict
  }
 where
   tracer :: Show x => String -> x -> x
   tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x
   -- receiveData :: WS.Connection -> IO (Maybe WS.Message)
   -- receiveData conn = WS.receiveData conn >>=
   --   \case SockegMsgData d -> pure $ Just d
   --         _ -> pure Nothing

main :: IO ()
main = do
    chan <- Chan.newChan
    withActiveConnection $ runApp chan
    Engine.start chan

client
  :: forall rej m a
  . (rej ~ Text, m ~ IO, a ~ SomeReply)
  => Tracer m String
  -> SomeRequest
  -> m (Client rej m a)
client tracer req = pure $ SendMsgRequest req handleReply
  where
    handleReply (Left rej) = do
      putStrLn $ "error: " <> Text.unpack rej
      pure SendMsgDone
    handleReply (Right rep) = do
      putStrLn $ show rep
      pure SendMsgDone
