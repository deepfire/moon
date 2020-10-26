{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Vty.Widget.Selector (module Reflex.Vty.Widget.Selector) where

import           Safe

import           Control.Arrow ((>>>), (&&&))
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId

import           Data.Function
import           Data.Functor ((<&>))
import           Data.List                             as List
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, unpack)
import           Data.Text.Zipper
import           Data.Text.Zipper.Extra

import           Reflex
import           Reflex.Network
import           Reflex.Vty.Widget                 hiding (text)
import           Reflex.Vty.Widget.Input.RichText
import           Reflex.Vty.Widget.Layout

import qualified Graphics.Vty as V

import           Basis hiding (Dynamic, left, right)

import Debug.Reflex

import Reflex.Vty.Widget.Extra


--------------------------------------------------------------------------------
-- * Selector:  completing input + menu
--
data Selector t m a b =
  Selector
  { selrSelection :: !(Event t (Selection a b)) --(SelectorFrameParams t m a b)
  , selrAccepted  :: !(Event t a)
  }

data SelectorFrameParams t m a b =
  SelectorFrameParams
  { sfpElems     :: ![a]
  , sfpCompletep :: !(Maybe Char -> Char -> Bool)
  , sfpSelection :: !(Selection a b)
  , sfpShow      :: !(a -> Text)
  , sfpPresent   :: !(a -> Behavior t Bool -> VtyWidget t m a)
  }

data Selection a b
  = Selection
  { selValue     :: !(Maybe a)
  , selIndex     :: !Index
  , selColumn    :: !Column
  , selCompleted :: !(Maybe Char)
  , selInput     :: !Text
  , selExt       :: !b
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

selector ::
  forall t m a b
  . (CWidget t m, Show a, Show b)
  => Reflex.Dynamic t (SelectorFrameParams t m a b)
  -> VtyWidget t m (Selector t m a b)
selector sfpD = mdo
  sfp0 <- sample (current sfpD)

  selectE :: Event t (Selection a b) <-
    switchDyn
      <$> (networkHold
            (selectorFrame sfp0)
            (selectorFrame <$> updated sfpD)
            :: VtyWidget t m (Dynamic t (Event t (Selection a b))))
    :: VtyWidget t m (Event t (Selection a b))

  pure Selector
    { selrSelection = selectE
    , selrAccepted  = fmapMaybe id $
                      selectE <&>
                        \Selection{..}->
                          join $ selCompleted $> selValue
    }
 where
   selectorFrame ::
     SelectorFrameParams t m a b ->
     VtyWidget t m (Event t (Selection a b))
   selectorFrame SelectorFrameParams{sfpSelection=Selection{..},
                                     sfpElems=sfpElems@(zip (fmap Index [0..])
                                              -> sfpIndexed), ..} = mdo

     -- widgets:  menu + incremental input
     ((_, menuChoiceD), offtInputE) <-
       splitV (pure (\x->x-1)) (pure $ join (,) True)
      (splitV (pure (\x->x-length sfpElems-2)) (pure $ join (,) True) (pure ())
        (menuWidget sfpIndexed))
        (inputWidget menuChoiceD)

     pure $
       attachPromptlyDyn menuChoiceD offtInputE
         <&> \(menuChoice :: Maybe (Index, a), (newCol, newInput, maybeCompl)) ->
              Selection
              { selValue     = menuChoice <&> snd
              , selColumn    = newCol
              , selInput     = newInput
              , selIndex     = menuChoice <&> fst & fromMaybe (Index 0)
              , selExt       = selExt
              , selCompleted = maybeCompl
              }
    where
      menuWidget ::
        [(Index, a)] -> VtyWidget t m (Dynamic t (Maybe (Index, a)))
      menuWidget xs =
        selectionMenu
          (\(ix, a) ->
             fmap (fmap (ix,) . fbFocused) . focusButton sfpPresent $ a)
          selIndex
          xs
      inputWidget :: Dynamic t (Maybe (Index, a)) -> VtyWidget t m (Event t (Column, Text, Maybe Char))
      inputWidget menuChoiceD =
        completingInput
          sfpCompletep ((sfpShow . snd <$>) <$> current menuChoiceD)
          "> " selInput selColumn

completingInput
  :: ReflexVty t m
  => (Maybe Char -> Char -> Bool) -- ^ Are we completing?
  -> Behavior t (Maybe Text)      -- ^ If user requests completion, that's what to.
  -> Text                         -- ^ Prompt.
  -> Text                         -- ^ Initial value.
  -> Column                       -- ^ Initial position.
  -> VtyWidget t m (Event t (Column, Text, Maybe Char))
completingInput completep completion prompt text0 (Column col0) = do
  width <- displayWidth
  TextInput sTxtD _ xyD <-
    row $ do
      fixedInert 2 $
        richTextStatic (foregro V.blue) (pure prompt)
      fixed (width - 2) $
        textInput completion $
          (defaultTextInputConfig Nothing)
          { _textInputConfig_initialValue = fromText text0 & top & rightN col0 & (Nothing,)
          , _textInputConfig_handler = interactorEventHandler
          }
  uSTxtD <- holdUniqDyn sTxtD
    -- ..now.. how does it change our model?
    -- ..as it probably fires coincidentally with txtD
    -- ..should it be an event, or should the TextZipper be just extended?
  pure . updated $
    zipDynWith lcons2
      (Column . fst <$> xyD)
      (swap <$> uSTxtD)
 where
   interactorEventHandler
     :: TextInputConfig t (Maybe Char) (Maybe Text)
     -> Int
     -> Maybe Text
     -> V.Event
     -> TextZipper -> (Maybe Char, TextZipper)
   interactorEventHandler TextInputConfig{..} pageSize mText = \case
     -- Special characters
     V.EvKey (V.KChar c) [] ->
       \z -> if completep (lookCharLeft z) c
             then (Just c, complete c mText z)
             else (Nothing, insertChar c z)
     ev -> ((Nothing,) .) $ case ev of
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

selectionMenu ::
  forall t m a
  . (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => (a -> VtyWidget t m (Event t a))
  -> Index
  -> [a]
  -> VtyWidget t m (Dynamic t (Maybe a))
selectionMenu displayMenuRow (Index selIx) =
  (id &&& displayPipeline)
  >>> \(xs, evW) ->
        evW >>= holdDyn (xs `atMay` selIx)
 where
   displayPipeline :: [a] -> VtyWidget t m (Event t (Maybe a))
   displayPipeline
     -- Display rows as a sequence of widgets,
     =   traverse (fixed 1 . displayMenuRow)
     -- Wrap in an arrow-navigable layout,
     >>> (\child -> do
             nav <- upDownNavigation
             runLayout (pure Orientation_Column) selIx nav child)
     -- Wrap in a box
     >>> boxStatic roundedBoxStyle
     -- -- Constrain in a pane
     -- >>> pane region (constDyn True)
     -- Merge lists of events:  XXX: efficiency?
     >>> fmap (fmap Just . leftmost)

instance (Show a, Show b) => Show (Selection a b) where
  show Selection{..} = unpack $ mconcat
    [ "#<Sel "
    , "i", pack . show $ unIndex selIndex, " "
    , "c", pack . show $ unColumn selColumn, " "
    , "v:", pack $ show selValue, " "
    , "e:", pack $ show selExt
    , ">"
    ]

emptySelection :: b -> Selection a b
emptySelection = Selection Nothing (Index 0) (Column 0) Nothing ""
