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
import           Control.Exception
import           Control.Lens ((.~))
import           Control.Tracer

import           NodeEditor.State.Global (State, pipes)

import Basis
import Pipe
import Type
import Lift.Client
import Wire.Protocol


data Event f
  = Pipes f

connect
  :: forall f
  . f ~ Event
  => URL
  -> MVar State
  -> IO ((Reply -> f Reply) -> Request -> IO ())
connect url state = do
  c <- mkConnection url (processor state)
  async $ do
    eres <- try (runConnectionWorkerIO c)
    case eres of
      Left (e :: SomeException) -> do
        putStrLn $ "Exception in Event.connect:  " <> show e
        throwIO e
      Right x -> pure x
  pure (request c)
 where
   handleAnyException :: SomeException -> IO ()
   handleAnyException = error . show

processor
  :: forall rej
   . (rej ~ Text)
  => MVar State
  -> Tracer IO String
  -> (Either rej (Event Reply))
  -> IO ()
processor _ tr (Left e) = do
  traceWith tr $ "Remote indicates error: " <> unpack e
  -- error (unpack e)
processor state tr (Right x) = do
  traceWith tr $ "Handling reply.."
  modifyMVar_ state (process x)
 where
   process :: Event Reply -> State -> IO State
   process (Pipes (ReplyValue s)) state =
     case withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) s
          (\(VPoint space) -> do
              traceWith tr $ "Updating pipes.."
              pure $ state & pipes .~ Just space)
     of Right x -> x
        Left e -> do
          traceWith tr (unpack e)
          pure state
