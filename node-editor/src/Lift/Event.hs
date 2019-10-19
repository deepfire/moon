{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
module Lift.Event
  ( Event(..)
  , connect
  )
where

import           Prelude

import           Control.Concurrent.Async (async)
import           Control.Concurrent.MVar (MVar, modifyMVar_)
import           Control.Lens ((.~))

import           NodeEditor.State.Global (State, pipes)

import Basis
import Lift.Client
import Wire.Protocol


data Event f
  = Pipes f

connect
  :: forall f
  . f ~ Event
  => URL
  -> MVar State
  -> IO ((SomeReply -> f SomeReply) -> SomeRequest -> IO ())
connect url state = do
  c <- mkConnection url (processor state)
  async $ runConnectionWorkerIO c
  pure (request c)

processor
  :: forall rej
   . (rej ~ Text)
  => MVar State
  -> (Either rej (Event SomeReply))
  -> IO ()
processor _ (Left e) = error (unpack e)
processor state (Right x) = error "UGH"
  -- modifyMVar_ state (process x)
  -- where
  --   process :: Event SomeReply -> State -> IO State
  --   process (Pipes s) state = do
  --     -- pure $
  --     --   state & pipes .~ undefined
      
  --     pure $ trace (printf "FOOOOOOOOOOOOO") (error "foo")
