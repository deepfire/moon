{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Event.Source
    ( AddHandler(..)
    , atomHandler
    , movementHandler
    , sceneResizeHandler
    ) where

import           Common.Prelude                    hiding (on)

import qualified Common.Batch.Connector.Connection as BatchConnection
import qualified JS.Atom                           as Atom
import qualified JS.Scene                          as Scene
import qualified NodeEditor.Event.Connection       as Connection
import           NodeEditor.Event.Event            (Event (Connection, UI))
import           NodeEditor.Event.UI               (UIEvent (AppEvent))
import qualified NodeEditor.React.Event.App        as App
import qualified WebSocket                         as WebSocket


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

atomHandler :: AddHandler Event
atomHandler = AddHandler Atom.onEvent

sceneResizeHandler :: AddHandler Event
sceneResizeHandler = AddHandler $ \h ->
    Scene.onSceneResize $ h $ UI $ AppEvent App.Resize

movementHandler :: AddHandler Event
movementHandler = AddHandler $ \h ->
    Scene.onMovement $ h . UI . AppEvent . App.Movement
