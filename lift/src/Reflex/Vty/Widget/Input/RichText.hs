{-|
Module: Reflex.Vty.Widget.Input.Text
Description: Widgets for accepting text input from users and manipulating text within those inputs
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Vty.Widget.Input.RichText
  ( module Reflex.Vty.Widget.Input.RichText
  , def
  ) where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.NodeId (MonadNodeId)
import Data.Default (Default(..))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget
-- import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Layout

-- | Configuration options for a 'textInput'. For more information on
-- 'TextZipper', see 'Data.Text.Zipper'.
data TextInputConfig t s a = TextInputConfig
  { _textInputConfig_initialValue :: (s, TextZipper)
  , _textInputConfig_modify :: Event t ((s, TextZipper) -> (s, TextZipper))
  , _textInputConfig_tabWidth :: Int
  , _textInputConfig_display :: Dynamic t (Char -> Char)
  -- ^ Transform the characters in a text input before displaying them. This is useful, e.g., for
  -- masking characters when entering passwords.
  , _textInputConfig_handler :: TextInputConfig t s a -> Int -> a -> V.Event -> TextZipper -> (s, TextZipper)
  }

--instance Reflex t => Default (TextInputConfig t a) where
defaultTextInputConfig :: Reflex t => b -> TextInputConfig t b a
defaultTextInputConfig s =
  TextInputConfig
  { _textInputConfig_initialValue = (s, "")
  , _textInputConfig_modify = never
  , _textInputConfig_tabWidth = 4
  , _textInputConfig_display = pure id
  , _textInputConfig_handler = updateTextZipper
  }

-- | The output produced by text input widgets, including the text
-- value and the number of display lines (post-wrapping). Note that some
-- display lines may not be visible due to scrolling.
data TextInput t b = TextInput
  { _textInput_value :: Dynamic t (b, Text)
  , _textInput_lines :: Dynamic t Int
  , _textInput_position :: Dynamic t (Int, Int)
    -- ^ Current cursor row and column.
  }

-- | A widget that allows text input
textInput
  :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m)
  => Behavior t a
  -> TextInputConfig t b a
  -> VtyWidget t m (TextInput t b)
textInput dyn cfg = do
  i <- input
  f <- focus
  dh <- displayHeight
  dw <- displayWidth
  rec bv <- foldDyn ($) (_textInputConfig_initialValue cfg) $
        mergeWith (.)
        [ attach (current dh) (attach dyn i)
          <&> \(dhV, (dynV, iV)) (_, tz) ->
                _textInputConfig_handler cfg cfg dhV dynV iV tz
        , _textInputConfig_modify cfg
        , let displayInfo = (,) <$> current rows <*> scrollTop
          in ffor (attach displayInfo click) $
               \((dl, st), MouseDown _ (mx, my) _) (b, tz) ->
                 (b, goToDisplayLinePosition mx (st + my) dl tz)
        ]
      click <- mouseDown V.BLeft
      let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
      let v = snd <$> bv
          rows = (\w s c -> displayLines w V.defAttr c s)
            <$> dw
            <*> (mapZipper <$> _textInputConfig_display cfg <*> v)
            <*> cursorAttrs
          img = images . _displayLines_spans <$> rows
      x <- holdUniqDyn $ T.length . _textZipper_before <$> v
      y <- holdUniqDyn $ _displayLines_cursorY <$> rows
      let newScrollTop :: Int -> (Int, Int) -> Int
          newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
      let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
      scrollTop <- hold 0 hy
      tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
  return $ TextInput
    { _textInput_value = (value <$>) <$> bv
    , _textInput_lines = length . _displayLines_spans <$> rows
    , _textInput_position = zipDyn x y
    }

-- | A widget that allows multiline text input
-- multilineTextInput
--   :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m)
--   => Behavior t a
--   -> TextInputConfig t b a
--   -> VtyWidget t m (TextInput t b)
-- multilineTextInput b cfg = do
--   i <- input
--   textInput b $ cfg
--     { _textInputConfig_modify = mergeWith (.)
--       [ fforMaybe i $ \case
--           V.EvKey V.KEnter [] -> Just $ insert "\n"
--           _ -> Nothing
--       , _textInputConfig_modify cfg
--       ]
--     }

-- | Wraps a 'textInput' or 'multilineTextInput' in a tile. Uses
-- the computed line count to greedily size the tile when vertically
-- oriented, and uses the fallback width when horizontally oriented.
textInputTile
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m (TextInput t b)
  -> Dynamic t Int
  -> Layout t m (TextInput t b)
textInputTile txt width = do
  o <- askOrientation
  rec t <- fixed sz txt
      let sz = join $ ffor o $ \case
            Orientation_Column -> _textInput_lines t
            Orientation_Row -> width
  return t

-- | Default attributes for the text cursor
cursorAttributes :: V.Attr
cursorAttributes = V.withStyle V.defAttr V.reverseVideo

-- | Turn a set of display line rows into a list of images (one per line)
images :: [[Span V.Attr]] -> [V.Image]
images = map (V.horizCat . map spanToImage)

-- | Turn a set of display line rows into a single image
image :: [[Span V.Attr]] -> V.Image
image = V.vertCat . images

-- | Turn a 'Span' into an 'Graphics.Vty.Image'
spanToImage :: Span V.Attr -> V.Image
spanToImage (Span attrs t) = V.text' attrs t

-- | Default vty event handler for text inputs
updateTextZipper
  :: TextInputConfig t b a
  -> Int -- ^ Page size
  -> a
  -> V.Event -- ^ The vty event to handle
  -> TextZipper -- ^ The zipper to modify
  -> (b, TextZipper)
updateTextZipper cf pageSize _ = ((fst $ _textInputConfig_initialValue cf,) .) .
 \case
  -- Special characters
  V.EvKey (V.KChar '\t') [] -> tab (_textInputConfig_tabWidth cf)
  -- Regular characters
  V.EvKey (V.KChar k) [] -> insertChar k
  -- Deletion buttons
  V.EvKey V.KBS [] -> deleteLeft
  V.EvKey V.KDel [] -> deleteRight
  -- Key combinations
  V.EvKey (V.KChar 'u') [V.MCtrl] -> const empty
  V.EvKey (V.KChar 'w') [V.MCtrl] -> deleteLeftWord
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
