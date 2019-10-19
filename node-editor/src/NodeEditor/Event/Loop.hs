module NodeEditor.Event.Loop where

import           Control.Concurrent.Chan  (Chan)
import qualified Control.Concurrent.Chan  as Chan
import           Control.Concurrent.MVar  (MVar)
import           Control.Monad            (forever)
import           Common.Prelude
import           NodeEditor.State.Global (State)

import Wire.Protocol

import Lift.Event


data LoopRef = LoopRef { _queue :: Chan (IO ())
                       , _state :: MVar State
                       , _req   :: (SomeReply -> Lift.Event.Event SomeReply) -> SomeRequest -> IO ()
                       }

makeLenses ''LoopRef

schedule :: LoopRef -> IO () -> IO ()
schedule loop = Chan.writeChan (loop ^. queue)

start :: Chan (IO ()) -> IO ()
start = forever . join . Chan.readChan
