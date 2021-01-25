module Reflex.Lift.Host (module Reflex.Lift.Host) where

import Control.Concurrent                           (forkIO, killThread)
import Control.Concurrent.Chan          qualified as Chan
import Control.Concurrent.Chan.Unagi    qualified as Unagi
import Control.Exception
import Control.Monad                                (forM, forM_, forever, unless)
import Control.Monad.Fix                            (fix)
import Control.Monad.IO.Class                       (liftIO, MonadIO)
import Control.Monad.Identity                       (Identity(..))
import Control.Monad.Ref                            (readRef)

import Data.Dependent.Sum                           (DSum ((:=>)))
import Data.IORef                                   (readIORef)
import Data.Maybe                                   (catMaybes)

import Reflex
import Reflex.Host.Class
import Reflex.Spider.Orphans ()

import Dom.Reflex
import Dom.RequestReply

import Debug.Trace


-- | Runs a Lift server for a particular client's event.
runLiftEventServer
  :: (forall (t :: *) (m :: * -> *)
      . MonadReflex t m
      => m (Event t ()))
  -- ^ A functional reactive Lift server.
  -> IO ()
runLiftEventServer liftServerGuest =

  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do

    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO Chan.newChan

    -- Run the vty "guest" application, providing the appropriate context. The
    -- result is a 'VtyResult', and a 'FireCommand' that will be used to
    -- trigger events.
    (liftServerResult, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the server to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the server to create new
                                          -- events and triggers and writes
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.

            liftServerGuest

    -- Reads the current value of the 'Picture' behavior and updates the
    -- display with it. This will be called whenever we determine that a
    -- display update is necessary. In this implementation that is when various
    -- events occur.
    -- let updateVty =
    --       sample (_vtyResult_picture vtyResult) >>= liftIO . V.update vty

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- After firing the post-build event, sample the vty result and update
    -- the display.
    -- updateVty

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent liftServerResult

    -- The main application loop. We wait for new events, fire those that
    -- have subscribers, and update the display. If we detect a shutdown
    -- request, the application terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $
        (Just <$> Chan.readChan events)
        `catches`
        [Handler $
         -- When there's nobody to post an event -- just quit.
         \BlockedIndefinitelyOnMVar ->
           pure Nothing]
      stops <-
        flip (maybe $ pure [True]) ers $
         \ers'-> do
          -- Fire events that have subscribers.
          fireEventTriggerRefs fc ers' $
            -- Check if the shutdown 'Event' is firing.
            readEvent shutdown >>= \case
              Nothing -> return False
              Just _  -> return True
      unless (or stops)
        loop                      -- Otherwise, update the display and loop.
  where
    -- TODO Some part of this is probably general enough to belong in reflex
    -- | Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: (Monad (ReadPhase m), MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a
