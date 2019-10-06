{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-missing-home-modules #-}
module Main where

import           Common.Prelude
import           Common.ClientId                          (clientId)
import           Control.Concurrent.Chan                  (Chan)
import qualified Control.Concurrent.Chan                as Chan
import           Control.Concurrent.MVar
import           Control.Exception
import qualified JS.Mount                               as Mount
import           NodeEditor.Event.Engine                  (LoopRef (LoopRef))
import qualified NodeEditor.Event.Event                 as Event
import qualified NodeEditor.Event.Engine                as Engine
import qualified NodeEditor.React.Model.App             as App
import qualified NodeEditor.React.Store                 as Store
import qualified NodeEditor.React.View.App              as App
import           NodeEditor.State.Global                  (mkState)
import qualified React.Flux                             as React
import           System.Random                            (newStdGen)
import           GHC.Stack                                (HasCallStack)

import           Control.Tracer
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Text                              as Text
import qualified JavaScript.WebSockets                  as WS
import qualified Network.TypedProtocol.Channel          as Net

import Basis
import Wire.Peer
import Wire.Protocol

runApp :: Chan (IO ()) -> WS.Connection -> IO ()
runApp concChan ws  = do
    random         <- newStdGen
    mdo
        let loop = LoopRef concChan state
            evScheduler = Engine.scheduleEvent loop
        evScheduler Event.Init
        appRef <- Store.createApp (App.mk Nothing) evScheduler
        React.reactRender Mount.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId random
        state <- newMVar initState
        Engine.connectEventSources ws loop
    App.focus

withActiveConnection :: HasCallStack => (WS.Connection -> IO ()) -> IO ()
withActiveConnection action = do
  WS.withUrl "ws://127.0.0.1:29671" $ \conn -> do
    action conn
    -- let tracer = stdoutTracer
    --     req = (SomeRequest $ Run "pipes")
    -- runClient tracer (client tracer req) $ channelFromWebsocket' conn
    -- WS.sendMessage conn (WS.SocketMsgText "LOL")

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
client _tracer req = pure $ SendMsgRequest req handleReply
  where
    handleReply (Left rej) = do
      putStrLn $ "error: " <> Text.unpack rej
      pure SendMsgDone
    handleReply (Right rep) = do
      putStrLn $ show rep
      pure SendMsgDone
