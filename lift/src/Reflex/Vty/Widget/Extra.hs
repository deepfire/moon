{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Vty.Widget.Extra
  ( module Reflex.Vty.Widget.Extra
  ) where

import           Safe

import           Control.Arrow ((***), (>>>), (&&&))
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId

import           Data.Function
import           Data.Functor ((<&>))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Zipper
import           Data.Text.Zipper.Extra
import           Data.List                             as List

import           Reflex
import           Reflex.Network
import           Reflex.Vty.Widget
import           Reflex.Vty.Widget.Input.RichText
import           Reflex.Vty.Widget.Layout

import qualified Graphics.Vty as V


type ReflexVty t m = (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, NotReady t m, PostBuild t m, MonadNodeId m)

menuSelector
  :: forall t m a b
  .  ( ReflexVty t m
     , Eq a, Show a)
  => DynRegion t
  -> Dynamic t ([a], a -> Behavior t Bool -> VtyWidget t m a)
  -> (a -> Text)
  -> VtyWidget t m (Event t (Maybe a, Int, Text))
menuSelector r feeDF presentText = mdo
  text0    <- pure ""
  inputD   <- holdDyn (Nothing, 0, text0) inputE
  let feeD  = zipDynWith (\(entries, present)
                           (mEntryIx, inputCol, inputText) ->
                            ( entries
                            , present
                            , inputText
                            , fromMaybe 0 $ join $ mEntryIx <&>
                                                   flip List.elemIndex entries
                            , inputCol))
              feeDF inputD
  (xs0, pres0, _text0, sel0, pos0)
           <- sample (current $ feeD)
  inputE   <- switchDyn <$>
              networkHold (frame r xs0 pres0 text0 sel0 pos0)
              (updated $
               feeD <&> \(xs, pres, text, sel, pos) ->
                           frame r xs  pres  text  sel  pos)

  pure inputE
 where
   frame :: DynRegion t
         -> [a] -> (a -> Behavior t Bool -> VtyWidget t m a)
         -> Text -> Int -> Int
         -> VtyWidget t m (Event t (Maybe a, Int, Text))
   frame r@(DynRegion l u w h) xs presentMenuRow text selIx pos = do
     let menuH = pure $ List.length xs + 2
         menuR = DynRegion l (u + h - menuH - 1) (zipDynWith min (w - 2) 50) menuH
         -- ruleRegion = DynRegion l (u + h - 7)         w       1
         intrR = DynRegion l (u + h - 1)         w       1

     selD       <- menu menuR (fmap snd . focusButton presentMenuRow)
                     selIx xs
     offtInputE <- completingInput intrR
                     ((presentText <$>) <$> current selD)
                     "> " text pos

     pure $ attachPromptlyDyn selD offtInputE
       <&> (\(a, (b, c)) -> (a, b, c))

completingInput
  :: ReflexVty t m
  => DynRegion t             -- ^ Widget's region
  -> Behavior t (Maybe Text) -- ^ If user requests completion, that's what to.
  -> Text                    -- ^ Prompt.
  -> Text                    -- ^ Initial value.
  -> Int                     -- ^ Initial position.
  -> VtyWidget t m (Event t (Int, Text))
completingInput r@(DynRegion _ _ rw _) completion prompt initial initialOfft = do
     TextInput txtD _ xyD <-
       pane r (pure True) $ row $ do
         fixedInert 2 $
           richTextStatic (foregro V.blue) (pure prompt)
         fixed (rw - 2) $
           textInput completion $ def
           { _textInputConfig_initialValue = fromText initial
                                             & top & rightN initialOfft
           , _textInputConfig_handler = interactorEventHandler
           }
     utxtD <- holdUniqDyn txtD
     pure $ updated $ zipDyn (fst <$> xyD) utxtD
 where
   interactorEventHandler
     :: TextInputConfig t (Maybe Text)
     -> Int
     -> Maybe Text
     -> V.Event
     -> (TextZipper -> TextZipper)
   interactorEventHandler TextInputConfig{..} pageSize mText = \case
     -- Special characters
     V.EvKey (V.KChar '\t') [] -> flip complete mText
     V.EvKey (V.KChar  ' ') [] -> flip complete mText
     -- Regular characters
     V.EvKey (V.KChar k) [] -> insertChar k
     -- Deletion buttons
     V.EvKey V.KBS [] -> deleteLeft
     V.EvKey V.KDel [] -> deleteRight
     -- Key combinations
     V.EvKey (V.KChar 'a') [V.MCtrl] -> home
     V.EvKey (V.KChar 'e') [V.MCtrl] -> end
     V.EvKey (V.KChar 'f') [V.MCtrl] -> right
     V.EvKey (V.KChar 'b') [V.MCtrl] -> left
     V.EvKey (V.KChar 'f') [V.MMeta] -> rightWord
     V.EvKey (V.KChar 'b') [V.MMeta] -> leftWord

     V.EvKey (V.KChar 'k') [V.MCtrl] -> killLine
     V.EvKey (V.KChar 'd') [V.MMeta] -> deleteRightWord
     V.EvKey (V.KChar 's') [V.MMeta] -> deleteLeftWord
     V.EvKey V.KBS [V.MMeta] -> deleteLeftWord
     -- V.EvKey V.KBS [V.MCtrl] -> TZ.deleteLeftWord -- jus doesn't work
     -- Arrow keys
     V.EvKey V.KLeft [] -> left
     V.EvKey V.KRight [] -> right
     V.EvKey V.KUp [] -> up
     V.EvKey V.KDown [] -> down
     V.EvKey V.KHome [] -> home
     V.EvKey V.KEnd [] -> end
     V.EvKey V.KPageUp [] -> pageUp pageSize
     V.EvKey V.KPageDown [] -> pageDown pageSize
     _ -> id

menu :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
        => DynRegion t
        -> (a -> VtyWidget t m (Event t a))
        -> Int
        -> [a]
        -> VtyWidget t m (Dynamic t (Maybe a))
menu region displayMenuRow selIx =
  (id &&& displayPipeline)
  >>> \(xs, evW) ->
        evW >>= holdDyn (xs `atMay` selIx)
 where
   displayPipeline
     -- Display rows as a sequence of widgets,
     =   traverse (fixed 1 . displayMenuRow)
     -- Wrap in an arrow-navigable layout,
     >>> (\child -> do
             nav <- upDownNavigation
             runLayout (pure Orientation_Column) selIx nav child)
     -- Wrap in a box
     >>> boxStatic roundedBoxStyle
     -- Constrain in a pane
     >>> pane region (constDyn True)
     -- Merge lists of events
     >>> fmap (fmap Just . leftmost)

upDownNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
upDownNavigation = do
  fwd <- fmap (const 1) <$> key V.KDown
  back <- fmap (const (-1)) <$> key V.KUp
  return $ leftmost [fwd, back]

selecting :: Reflex t => (V.Attr -> V.Attr) -> Behavior t V.Attr -> Behavior t Bool -> Behavior t V.Attr
selecting attrXform attrB selB = comp <$> attrB <*> selB
  where
    comp attr False = attr
    comp attr True  = attrXform attr

richTextStatic :: ReflexVty t m => V.Attr -> Behavior t Text -> VtyWidget t m ()
richTextStatic = richText . RichTextConfig . pure

fixedInert
  :: (Reflex t, Monad m, MonadNodeId m)
  => Dynamic t Int
  -> VtyWidget t m a
  -> Layout t m a
fixedInert sz =
  tile (TileConfig
         { _tileConfig_constraint = Constraint_Fixed <$> sz
         , _tileConfig_focusable  = pure False
         })
  . clickable

clickable
  :: (Reflex t, Monad m)
  => VtyWidget t m a
  -> VtyWidget t m (Event t (), a)
clickable child = do
  click <- mouseDown V.BLeft
  a <- child
  return (() <$ click, a)

focusButton
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => (a -> Behavior t Bool -> VtyWidget t m a)
  -> a
  -> VtyWidget t m (Event t a, Event t a)
focusButton child a = do
  f <- focus
  focused <- scanDynMaybe
             (const (False, a))
             (curry $ \case
                 (True, (False, _)) -> Just (True, a)
                 _ -> Nothing)
             f
  child a (current f)
  m <- mouseUp
  k <- key V.KEnter
  return $ (leftmost [a <$ k, a <$ m], a <$ updated focused)

foregro :: V.Color -> V.Attr
foregro = V.withForeColor V.defAttr
