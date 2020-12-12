{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Vty.Widget.Extra
  ( module Reflex.Vty.Widget.Extra
  ) where

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
import           Reflex.Vty.Widget
import           Reflex.Vty.Widget.Input.RichText
import           Reflex.Vty.Widget.Layout

import qualified Graphics.Vty as V

import           Basis hiding (Dynamic, left, right)

import Debug.Reflex


type ReflexVty t m =
  ( Adjustable t m
  , MonadFix m
  , MonadHold t m
  , MonadNodeId m
  , NotReady t m
  , PostBuild t m
  , Reflex t
  )

newtype Width  = Width  { unWidth'  :: Int } deriving (Eq, Enum, Num, Ord, Real, Integral)
newtype Height = Height { unHeight' :: Int } deriving (Eq, Enum, Num, Ord, Real, Integral)
newtype Index  = Index  { unIndex'  :: Int } deriving Show
newtype Column = Column { unColumn' :: Int } deriving Show

unWidth :: Width -> Int
unWidth = unWidth'
{-# INLINE unWidth #-}

unHeight :: Height -> Int
unHeight = unHeight'
{-# INLINE unHeight #-}

unColumn :: Column -> Int
unColumn = unColumn'
{-# INLINE unColumn #-}

unIndex :: Index -> Int
unIndex = unIndex'
{-# INLINE unIndex #-}

-- XXX:  efficiency of fmap (fmap Just . leftmost) over 100+ elts?

upDownNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
upDownNavigation = do
  fwd  <- fmap (const   1)  <$> key V.KDown
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

exitOnTheEndOf :: ReflexVty t m => VtyWidget t m (Event t V.Event) -> VtyWidget t m (Event t ())
exitOnTheEndOf inp =
  inp <&>
    fmapMaybe
    \case
      V.EvKey (V.KChar 'c') [V.MCtrl]
        -> Just () -- terminate
      _ -> Nothing

--------------------------------------------------------------------------------
-- * Attributery
--
foregro :: V.Color -> V.Attr
foregro = V.withForeColor V.defAttr

red, blue, green, yellow, white, grey :: V.Attr
(,,,,,) red blue green yellow white grey =
 (,,,,,) (foregro V.red) (foregro V.blue) (foregro V.green) (foregro V.yellow)
         (foregro V.white) (foregro V.brightGreen)

--------------------------------------------------------------------------------
-- * Basic
--

--------------------------------------------------------------------------------
-- * Focus button
--
data FocusButton t a =
  FocusButton
  { fbPress   :: !(Event t a)
  , fbFocused :: !(Event t a)
  }

richTextFocusConfigDef :: Reflex t => Behavior t Bool -> RichTextConfig t
richTextFocusConfigDef = richTextFocusConfig V.defAttr

richTextFocusConfig :: Reflex t => V.Attr -> Behavior t Bool -> RichTextConfig t
richTextFocusConfig attr focB =
  RichTextConfig $
    selecting (flip V.withBackColor $ V.rgbColor @Integer 1 1 1)
              (pure attr)
              focB

buttonPresentText ::
     (Monad m, Reflex t)
  => (Behavior t Bool -> RichTextConfig t)
  -> (a -> Text)
  -> a
  -> Behavior t Bool
  -> VtyWidget t m a
buttonPresentText focusStyle showT x focusB =
  richText (focusStyle focusB) (pure $ showT x)
  >> pure x

  -- XXX: this is a bullshit button -- doesn't fire focus immediately
focusButton ::
     (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => (a -> Behavior t Bool -> VtyWidget t m a)
  -> a
  -> VtyWidget t m (FocusButton t a)
focusButton renderChild a = do
  f <- focus
  focused <- scanDynMaybe
             (const (False, a))
             (curry $ \case
                 (True, (False, _)) -> Just (True, a)
                 _ -> Nothing)
             f
  void $ renderChild a (current f)
  m <- mouseUp
  k <- key V.KEnter
  pure FocusButton
    { fbPress   = leftmost [a <$ k, a <$ m]
    , fbFocused = a <$ updated focused
    }
