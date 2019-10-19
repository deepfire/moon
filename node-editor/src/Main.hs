{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-missing-home-modules #-}
module Main where

import           Common.Prelude
import           Common.ClientId                          (clientId)
import qualified Control.Concurrent.Chan                as Chan
import           Control.Concurrent.MVar
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

import Wire.Protocol

import Lift.Client
import Lift.Event


main :: IO ()
main = do
    chan <- Chan.newChan
    random <- newStdGen
    mdo
        let loop = LoopRef chan state request
            evScheduler = Engine.scheduleEvent loop
        evScheduler Event.Init
        appRef <- Store.createApp (App.mk Nothing) evScheduler
        React.reactRender Mount.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId random
        state <- newMVar initState
        request <- connect (URL "ws://127.0.0.1:29671") state
        -- Engine.connectEventSources ws loop
        request Pipes (SomeRequest $ Run "meta.space")
        pure ()
    App.focus
    Engine.start chan
