{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -threaded #-}

import Control.Arrow
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import Data.Monoid (appEndo)
import Data.Semigroup (getFirst)
import Control.Applicative (liftA2)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (onException)
import Control.Monad (forM, forM_, forever, join, void)
import Control.Monad.IO.Class
import Control.Monad.Ref (readRef)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Fix
import Control.Monad.NodeId
import Data.Dependent.Sum (DSum (..))
import Data.IORef (readIORef)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text.Zipper as TZ
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Graphics.Vty as V
import Reflex
import Reflex.Host.Class
import Reflex.Network
import Reflex.Vty
import System.Environment (getArgs)

import Debug.Trace (trace)
import Debug.Reflex
import Text.Printf (printf)


exhibit1
  :: forall t m
  . ( Adjustable t m
    , MonadFix m, MonadHold t m, MonadNodeId m
    , PerformEvent t m, PostBuild t m
    , MonadIO (Performable m)
    , Reflex t
    , TriggerEvent t m)
  => [Text] -> VtyWidget t m ()
exhibit1 xs = mdo
  periodically <- tickLossyFromPostBuildTime 1
  ev <- updated <$> count never
  -- simulate user "input"
  let filtE = T.isInfixOf . (["a", "b"] !!) . flip mod 2 <$> ev

  filtDE :: Dynamic t (Event t (Text -> Bool)) <-
    networkHold
    (menuiseWithInput xs)
    (menuiseWithInput . (flip filter xs)
     <$>
      traceErrEventWith (const "rebuild") filtE)
  pure ()
 where menuiseWithInput xs = menuise xs >> finput ""

exhibit2
  :: forall t m
  . ( Adjustable t m
    , MonadFix m, MonadHold t m, MonadNodeId m
    , PerformEvent t m, PostBuild t m
    , MonadIO (Performable m)
    , Reflex t
    , TriggerEvent t m)
  => [Text] -> VtyWidget t m ()
exhibit2 xs = mdo
  filtDE :: Dynamic t (Event t (Text, Text -> Bool)) <-
    networkHold
    (menuiseWithInput "" xs)
    ((\(flttext, flt) ->
        menuiseWithInput flttext $ filter flt xs)
     <$>
      (traceErrEventWith (const "rebuild") <$> switch $ current filtDE))
  pure ()
 where
   menuiseWithInput :: Text -> [Text] -> VtyWidget t m (Event t (Text, Text -> Bool))
   menuiseWithInput flttext xs = menuise xs >> (traceErrEventWith (const "finput") <$> finput' flttext)

exhibit3
  :: forall t m
  . ( Adjustable t m
    , MonadFix m, MonadHold t m, MonadNodeId m
    , PerformEvent t m, PostBuild t m
    , MonadIO (Performable m)
    , Reflex t
    , NotReady t m
    , TriggerEvent t m)
  => [Text] -> VtyWidget t m ()
exhibit3 xs0 = mdo
  flt <- finput ""
  xs <- descs xs0 flt
  pure ()

finput
  :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => Text -> VtyWidget t m (Event t (Text -> Bool))
finput init = do
  TextInput txtD _ <- getInput init
  pure $ T.isInfixOf <$> updated txtD

finput'
  :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => Text -> VtyWidget t m (Event t (Text, Text -> Bool))
finput' init = do
  TextInput txtD _ <- getInput init
  utxtD <- holdUniqDyn txtD
  pure $ (id &&& T.isInfixOf) <$> updated utxtD

finputDyn
  :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => Text -> VtyWidget t m (Dynamic t (Text -> Bool))
finputDyn init = do
  TextInput txtD _ <- getInput init
  holdDyn (const True) $ ffor (updated txtD) $
    \t-> (t `T.isInfixOf`)

menuise
  :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => [Text]
  -> VtyWidget t m [Event t ()]
menuise = pane' (DynRegion 0 1  20 9) (constDyn True)
          . (\child -> do
                nav <- tabNavigation
                runLayout' (pure Orientation_Column) 0 nav child)
          . traverse (fixed 3 . textButtonStatic def)

runVtyWidget'
  :: (Reflex t, MonadNodeId m)
  => VtyWidgetCtx t
  -> VtyWidget t m a
  -> m (a, Behavior t [V.Image])
runVtyWidget' ctx w = runReaderT (runBehaviorWriterT (unVtyWidget w)) ctx

runVtyAppWithHandle'
  :: V.Vty
  -- ^ A 'Graphics.Vty.Vty' handle.
  -> (forall t m. VtyApp t m)
  -- ^ A functional reactive vty application.
  -> IO ()
runVtyAppWithHandle' vty vtyGuest = flip onException (V.shutdown vty) $

  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do

    -- Create an 'Event' and a "trigger" reference for that event. The trigger
    -- reference can be used to determine whether anyone is "subscribed" to
    -- that 'Event' and, therefore, whether we need to bother performing any
    -- updates when the 'Event' fires.
    -- The 'Event' below will be used to convey vty input events.
    (vtyEvent, vtyEventTriggerRef) <- newEventWithTriggerRef

    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan

    displayRegion0 <- V.displayBounds $ V.outputIface vty

    -- Run the vty "guest" application, providing the appropriate context. The
    -- result is a 'VtyResult', and a 'FireCommand' that will be used to
    -- trigger events.
    (vtyResult, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the guest app to create new
                                          -- events and triggers and writes
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.

            vtyGuest displayRegion0 vtyEvent
                                          -- The guest app is provided the
                                          -- initial display region and an
                                          -- 'Event' of vty inputs.

    -- Reads the current value of the 'Picture' behavior and updates the
    -- display with it. This will be called whenever we determine that a
    -- display update is necessary. In this implementation that is when various
    -- events occur.
    let updateVty = do
          pic <- sample (_vtyResult_picture vtyResult)
          liftIO $ do
            V.update vty pic

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- After firing the post-build event, sample the vty result and update
    -- the display.
    updateVty

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent $ _vtyResult_shutdown vtyResult

    -- Fork a thread and continuously get the next vty input event, and then
    -- write the input event to our channel of FRP 'Event' triggers.
    -- The thread is forked here because 'nextEvent' blocks.
    nextEventThread <- liftIO $ forkIO $ forever $ do
      -- Retrieve the next input event.
      ne <- V.nextEvent vty
      let -- The reference to the vty input 'EventTrigger'. This is the trigger
          -- we'd like to associate the input event value with.
          triggerRef = EventTriggerRef vtyEventTriggerRef
          -- Create an event 'TriggerInvocation' with the value that we'd like
          -- the event to have if it is fired. It may not fire with this value
          -- if nobody is subscribed to the 'Event'.
          triggerInvocation = TriggerInvocation ne $ return ()
      -- Write our input event's 'EventTrigger' with the newly created
      -- 'TriggerInvocation' value to the queue of events.
      writeChan events $ [triggerRef :=> triggerInvocation]

    -- The main application loop. We wait for new events, fire those that
    -- have subscribers, and update the display. If we detect a shutdown
    -- request, the application terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
      if or stop
        then liftIO $ do             -- If we received a shutdown 'Event'
          killThread nextEventThread -- then stop reading input events and
          V.shutdown vty             -- call the 'Graphics.Vty.Vty's shutdown command.

        else do                      -- Otherwise, update the display and loop.
          updateVty
          loop
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

-- | Sets up the top-level context for a 'VtyWidget' and runs it with that context
mainWidgetWithHandle'
  :: V.Vty
  -> (forall t m. (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Event t ()))
  -> IO ()
mainWidgetWithHandle' vty child =
  runVtyAppWithHandle' vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_width = fmap fst size
          , _vtyWidgetCtx_height = fmap snd size
          , _vtyWidgetCtx_input = inp'
          , _vtyWidgetCtx_focus = constDyn True
          }
    (shutdown, images) <- runNodeIdT $ runVtyWidget ctx $ do
      tellImages . ffor (current size) $ \(w, h) -> [V.charFill V.defAttr ' ' w h]
      traceErrEventWith (const "child") <$> child
    return $ VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) images
      , _vtyResult_shutdown = shutdown
      }

-- | Like 'mainWidgetWithHandle', but uses a default vty configuration
mainWidget'
  :: (forall t m. (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Event t ()))
  -> IO ()
mainWidget' child = do
  vty <- getDefaultVty
  mainWidgetWithHandle' vty child

runLayout'
  :: (MonadFix m, MonadHold t m, PostBuild t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Int -- ^ The positional index of the initially focused tile
  -> Event t Int -- ^ An event that shifts focus by a given number of tiles
  -> Layout t m a -- ^ The 'Layout' widget
  -> VtyWidget t m a
runLayout' ddir focus0 focusShift (Layout child) = do
  dw <- displayWidth
  dh <- displayHeight
  let mainAxis = ffor3 ddir dw dh $ \d w h -> case d of
        Orientation_Column -> h
        Orientation_Row -> w
  pb <- getPostBuild
  rec ((a, focusReq), queriesEndo) <- runReaderT (runDynamicWriterT $ runEventWriterT child) $ LayoutCtx solutionMap focusDemux ddir
      let queries = flip appEndo [] <$> queriesEndo
          solution = ffor2 mainAxis queries $ \sz qs -> Map.fromList
            . Map.elems
            . computeEdges
            . computeSizes sz
            . fmap (fmap snd)
            . Map.fromList
            . zip [0::Integer ..]
            $ qs
          solutionMap = ffor solution $ \ss -> ffor ss $ \(offset, sz) -> LayoutSegment
            { _layoutSegment_offset = offset
            , _layoutSegment_size = sz
            }
          focusable = fmap (Bimap.fromList . zip [0..]) $
            ffor queries $ \qs -> fforMaybe qs $ \(nodeId, (f, _)) ->
              if f then Just nodeId else Nothing
          adjustFocus
            :: (Bimap Int NodeId, (Int, Maybe NodeId))
            -> Either Int NodeId
            -> (Int, Maybe NodeId)
          adjustFocus (fm, (cur, _)) (Left shift) =
            let ix = (cur + shift) `mod` (max 1 $ Bimap.size fm)
            in (ix, Bimap.lookup ix fm)
          adjustFocus (fm, (cur, _)) (Right goto) =
            let ix = fromMaybe cur $ Bimap.lookupR goto fm
            in (ix, Just goto)
          focusChange = attachWith
            adjustFocus
            (current $ (,) <$> focusable <*> focussed)
            $ leftmost
            [ Left <$> focusShift
            , Left 0 <$ pb
            , Right . getFirst <$> focusReq
            ]
      -- A pair (Int, Maybe NodeId) which represents the index
      -- that we're trying to focus, and the node that actually gets
      -- focused (at that index) if it exists
      focussed <-
        traceErrDynWith
        (\(ix, _) -> "focussed "<>show ix) <$>
        holdDyn (focus0, Nothing) focusChange
      let focusDemux = demux $ snd <$> focussed
  return a

descs
   :: forall t m.
     ( MonadHold t m
     , MonadFix m
     , Reflex t
     , Adjustable t m
     , NotReady t m
     , PostBuild t m
     , MonadNodeId m
     )
  => [Text]
  -> Event t (Text -> Bool)
  -> VtyWidget t m (Dynamic t (Map.Map Int (Dynamic t Text)))
descs descs0 newFilter = do
  let descsMap0 = Map.fromList $ zip [0..] descs0
  rec tabNav <- tabNavigation
      let tileCfg = def { _tileConfig_constraint = pure $ Constraint_Fixed 1}
      listOut <- runLayout (pure Orientation_Column) 0 tabNav $
        listHoldWithKey descsMap0 updates $ \k t -> tile tileCfg $ do
          click <- void <$> mouseDown V.BLeft
          pb <- getPostBuild
          let focusMe = leftmost [ click, pb ]
          r <- pure t <$ textButtonStatic def t
          return (focusMe, r)
      let updates = ffor (attach (current descsMap) newFilter) $
            \(descs', flt) -> descs' <&> \v -> if flt v then Just v else Nothing
          -- updates = leftmost [insert, delete]
          -- delete = ffor todoDelete $ \k -> Map.singleton k Nothing
          -- todoDelete = switch . current $
          --   leftmost .  Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete) <$> listOut
          descsMap = joinDynThroughMap listOut
  return listOut

pane'
  :: (Reflex t, Monad m, MonadNodeId m)
  => DynRegion t
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> VtyWidget t m a
  -> VtyWidget t m a
pane' dr foc child = VtyWidget $ do
  ctx <- lift ask
  let reg = currentRegion dr
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = leftmost -- TODO: think about this leftmost more.
            [ fmapMaybe id $
                attachWith (\(r,f) e -> filterInput r f e)
                  (liftA2 (,) reg (current foc))
                  (_vtyWidgetCtx_input ctx)
            ]
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) foc
        , _vtyWidgetCtx_width = _dynRegion_width dr
        , _vtyWidgetCtx_height = _dynRegion_height dr
        }
  (result, images) <- lift . lift $ runVtyWidget ctx' child
  let images' = liftA2 (\r is -> map (withinImage r) is) reg images
  tellImages images'
  return result
  where
    filterInput :: Region -> Bool -> VtyEvent -> Maybe VtyEvent
    filterInput (Region l t w h) focused e = case e of
      V.EvKey _ _ | not focused -> Nothing
      V.EvMouseDown x y btn m -> mouse (\u v -> V.EvMouseDown u v btn m) x y
      V.EvMouseUp x y btn -> mouse (\u v -> V.EvMouseUp u v btn) x y
      _ -> Just e
      where
        mouse con x y
          | or [ x < l
               , y < t
               , x >= l + w
               , y >= t + h ] = Nothing
          | otherwise =
            Just (con (x - l) (y - t))
    withinImage
      :: Region
      -> V.Image
      -> V.Image
    withinImage (Region left top width height)
      | width < 0 || height < 0 = withinImage (Region left top 0 0)
      | otherwise = V.translate left top . V.crop width height

main :: IO ()
main = do
  args <- getArgs
  mainWidget' $ do
    inp <- input
    let xs = ["aa", "ab", "ac"]
        choice = T.pack . fromMaybe "2" $ listToMaybe args
    text . pure $ "Exhibit " <> choice
    pane' (DynRegion 0 0 20 20) (constDyn True) $
      case choice of
        "1" -> exhibit1 xs
        "2" -> exhibit2 xs
        "3" -> exhibit2 xs
        _ -> error "Usage:  exhibit [1 | 2 | 3]"
    return $ fforMaybe inp $ \case
      V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
      _ -> Nothing

getInput
  :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => Text -> VtyWidget t m (TextInput t)
getInput init = pane' (DynRegion 0 10 80 1) (pure True) $ col $ fixed 1 $ textInput $
  def { _textInputConfig_initialValue = TZ.fromText init }
