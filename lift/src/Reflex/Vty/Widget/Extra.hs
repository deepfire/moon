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
import           Reflex.Vty.Widget                 hiding (text)
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

newtype Width  = Width  { unWidth  :: Int }
newtype Height = Height { unHeight :: Int }
newtype Index  = Index  { unIndex  :: Int } deriving Show
newtype Column = Column { unColumn :: Int } deriving Show

instance (Show a, Show b) => Show (Selection a b) where
  show Selection{..} = unpack $ mconcat
    [ "#<Sel "
    , "i", pack . show $ unIndex selIndex, " "
    , "c", pack . show $ unColumn selColumn, " "
    , "v:", pack $ show selValue, " "
    , "e:", pack $ show selExt
    , ">"
    ]

data Selection a b
  = Selection
  { selValue     :: !(Maybe a)
  , selIndex     :: !Index
  , selColumn    :: !Column
  , selCompleted :: !(Maybe Char)
  , selInput     :: !Text
  , selExt       :: !b
  }

data SelectorFrameParams t m a b =
  SelectorFrameParams
  { sfpElems     :: ![a]
  , sfpCompletep :: !(Maybe Char -> Char -> Bool)
  , sfpSelection :: !(Selection a b)
  , sfpShow      :: !(a -> Text)
  , sfpPresent   :: !(a -> Behavior t Bool -> VtyWidget t m a)
  }

data Selector t m a b =
  Selector
  { selrSelection :: !(Event t (Selection a b)) --(SelectorFrameParams t m a b)
  , selrChoice    :: !(Event t a)
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

emptySelection :: b -> Selection a b
emptySelection = Selection Nothing (Index 0) (Column 0) Nothing ""

selector ::
  forall t m a b
  . (CWidget t m, Eq a, Show a, Show b)
  => DynRegion t
  -> Reflex.Dynamic t (SelectorFrameParams t m a b)
  -> VtyWidget t m (Selector t m a b)
selector (DynRegion l u w h) sfpD = mdo
  sfp0 <- sample (current sfpD)

  selectE <-
    switchDyn
      <$> (networkHold
            (selectorFrame sfp0)
            (selectorFrame <$> updated sfpD)
            :: VtyWidget t m (Dynamic t (Event t (Selection a b))))
    :: VtyWidget t m (Event t (Selection a b))

  pure Selector
    { selrSelection = selectE
    , selrChoice    = fmapMaybe id $
                      selectE <&>
                        \Selection{..}->
                          join $ selCompleted $> selValue
    }
 where
   selectorFrame ::
     SelectorFrameParams t m a b ->
     VtyWidget t m (Event t (Selection a b))
   selectorFrame SelectorFrameParams{sfpSelection=Selection{..}, ..} = do

     menuChoiceD :: Dynamic t (Maybe a) <-
       menu menuReg (fmap fbFocused . focusButton sfpPresent)
            selIndex sfpElems

     offtInputE :: Event t (Column, Text, Maybe Char) <-
       completingInput inputReg
                       sfpCompletep
                       ((sfpShow <$>) <$> current menuChoiceD)
                       "> " selInput selColumn

     pure $
       attachPromptlyDyn menuChoiceD offtInputE
         <&> \(menuChoice, (newCol, newInput, maybeCompl)) ->
              Selection
              { selValue  = menuChoice
              , selColumn = newCol
              , selInput  = newInput
              , selIndex  = menuChoice  <&>
                            (flip List.elemIndex sfpElems
                             >>> fmap Index)
                            & join
                            & fromMaybe (Index 0)
              , selExt    = selExt
              , selCompleted = maybeCompl
              }
    where
      menuH    = pure $ List.length sfpElems + 2
      menuReg  = DynRegion l (u + h - menuH - 1) (w - 2) menuH
      inputReg = DynRegion l (u + h - 1)          w      1

completingInput
  :: ReflexVty t m
  => DynRegion t                  -- ^ Widget's region
  -> (Maybe Char -> Char -> Bool) -- ^ Are we completing?
  -> Behavior t (Maybe Text)      -- ^ If user requests completion, that's what to.
  -> Text                         -- ^ Prompt.
  -> Text                         -- ^ Initial value.
  -> Column                       -- ^ Initial position.
  -> VtyWidget t m (Event t (Column, Text, Maybe Char))
completingInput r@(DynRegion _ _ rw _)
 completep completion prompt text0 (Column col0) = do
  TextInput sTxtD _ xyD <-
    pane r (pure True) $ row $ do
      fixedInert 2 $
        richTextStatic (foregro V.blue) (pure prompt)
      fixed (rw - 2) $
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

-- XXX:  efficiency of fmap (fmap Just . leftmost) over 100+ elts?
menu ::
  forall t m a
  . (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => DynRegion t
  -> (a -> VtyWidget t m (Event t a))
  -> Index
  -> [a]
  -> VtyWidget t m (Dynamic t (Maybe a))
menu region displayMenuRow (Index selIx) =
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
     -- Constrain in a pane
     >>> pane region (constDyn True)
     -- Merge lists of events:  XXX: efficiency?
     >>> fmap (fmap Just . leftmost)

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

-- * Focus button
--
data FocusButton t a =
  FocusButton
  { fbPress   :: !(Event t a)
  , fbFocused :: !(Event t a)
  }

focusButton
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => (a -> Behavior t Bool -> VtyWidget t m a)
  -> a
  -> VtyWidget t m (FocusButton t a)
focusButton child a = do
  f <- focus
  focused <- scanDynMaybe
             (const (False, a))
             (curry $ \case
                 (True, (False, _)) -> Just (True, a)
                 _ -> Nothing)
             f
  void $ child a (current f)
  m <- mouseUp
  k <- key V.KEnter
  pure FocusButton
    { fbPress   = leftmost [a <$ k, a <$ m]
    , fbFocused = a <$ updated focused
    }

-- * Attributetry
--
foregro :: V.Color -> V.Attr
foregro = V.withForeColor V.defAttr
