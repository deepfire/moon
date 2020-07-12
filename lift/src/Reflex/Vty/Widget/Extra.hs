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
import qualified Data.Text as T
import           Data.Text.Zipper
import           Data.Text.Zipper.Extra

import           Reflex
import           Reflex.Network
import           Reflex.Vty.Widget                 hiding (text)
import           Reflex.Vty.Widget.Input.RichText
import           Reflex.Vty.Widget.Layout

import qualified Graphics.Vty as V

import           Basis (lcons2, uncurry3)


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

data Selection a b
  = Selection
  { selValue     :: !(Maybe a)
  , selIndex     :: !Index
  , selColumn    :: !Column
  , selCompleted :: !Bool
  , selInput     :: !Text
  , selExt       :: !b
  }
instance Show a => Show (Selection a b) where
  show Selection{..} = unpack $ mconcat
    [ "#<Sel "
    , "_", selInput, "_ "
    , "ix=", pack . show $ unIndex selIndex, " "
    , "col=", pack . show $ unColumn selColumn, " "
    , "cmp=", pack . show $ selCompleted, " "
    , "val=", pack $ show selValue
    , ">"
    ]

data SelectorFrameParams t m a b =
  SelectorFrameParams
  { sfpElems     :: ![a]
  , sfpSelection :: !(Selection a b)
  , sfpShow      :: !(a -> Text)
  , sfpPresent   :: !(a -> Behavior t Bool -> VtyWidget t m a)
  }

data Selector t a b =
  Selector
  { selrSelection :: !(Event t (Selection a b))
  , selrChoice    :: !(Event t (Maybe a))
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

emptySelection :: b -> Selection a b
emptySelection = Selection Nothing (Index 0) (Column 0) False ""

selector ::
  forall t m a b
  . (CWidget t m, Eq a, Show a, Monoid b)
  => DynRegion t
  -> (Width -> [a] -> Selection a b -> SelectorFrameParams t m a b)
  -> Reflex.Dynamic t [a]
  -> VtyWidget t m (Selector t a b)
selector region@(DynRegion l u w h) computeSelectorFrameParams xsD = mdo
  let noSelection = emptySelection mempty
  sfp0 <-
    computeSelectorFrameParams
      <$> (Width <$> sample (current $ _dynRegion_width region))
      <*> sample (current xsD)
      <*> pure noSelection

  selectE <-
    switchDyn
      <$> (networkHold
            (selectorFrame sfp0)
            (selectorFrame . uncurry3 computeSelectorFrameParams
              <$> attachPromptlyDynWith lcons2
                  (Width <$> _dynRegion_width region)
                  (attachPromptlyDynWith (,)
                   xsD
                   selectE))
            :: VtyWidget t m (Dynamic t (Event t (Selection a b))))
    :: VtyWidget t m (Event t (Selection a b))

  pure Selector
    { selrSelection = selectE
    -- XXX: we don't have nice text input facilities yet (that tell us about completion),
    --      and so we must detect if it happened
    , selrChoice    = fforMaybe selectE $
                      \Selection{..}->
                        if selCompleted
                        then Just selValue
                        else Nothing
    }
 where
   selectorFrame :: SelectorFrameParams t m a b
                 -> VtyWidget t m (Event t (Selection a b))
   selectorFrame SelectorFrameParams{sfpSelection=Selection{..}, ..} = do
     let menuH    = pure $ List.length sfpElems + 2
         menuReg  = DynRegion l (u + h - menuH - 1) (w - 2) menuH
         inputReg = DynRegion l (u + h - 1)          w      1

     menuChoiceD :: Dynamic t (Maybe a) <-
       menu menuReg (fmap fbFocused . focusButton sfpPresent)
                     selIndex sfpElems
     offtInputE :: Event t (Column, Text) <-
       completingInput inputReg ((sfpShow <$>) <$> current menuChoiceD)
                       "> " selInput selColumn

     let selectionE = attachPromptlyDyn menuChoiceD offtInputE
           <&> \(val, (newCol, newInput)) ->
                 Selection
                 { selValue  = val
                 , selColumn = newCol
                 , selInput  = newInput
                 , selIndex  = val <&>
                               (flip List.elemIndex sfpElems
                                >>> fmap Index)
                               & join
                               & fromMaybe (Index 0)
                 , selExt    = selExt
                 , selCompleted =
                   let strIx = unColumn newCol - 1
                   in if strIx < T.length newInput && strIx >= 0
                      then T.index newInput strIx == ' '
                      else False
                 }
     pure selectionE

completingInput
  :: ReflexVty t m
  => DynRegion t             -- ^ Widget's region
  -> Behavior t (Maybe Text) -- ^ If user requests completion, that's what to.
  -> Text                    -- ^ Prompt.
  -> Text                    -- ^ Initial value.
  -> Column                  -- ^ Initial position.
  -> VtyWidget t m (Event t (Column, Text))
completingInput r@(DynRegion _ _ rw _) completion prompt text0 (Column col0) = do
  TextInput txtD _ xyD <-
    pane r (pure True) $ row $ do
      fixedInert 2 $
        richTextStatic (foregro V.blue) (pure prompt)
      fixed (rw - 2) $
        textInput completion $
          def
          { _textInputConfig_initialValue = fromText text0 & top & rightN col0
          , _textInputConfig_handler = interactorEventHandler
          }
  utxtD <- holdUniqDyn txtD
  pure $ updated $ zipDyn (Column . fst <$> xyD) utxtD
 where
   interactorEventHandler
     :: TextInputConfig t (Maybe Text)
     -> Int
     -> Maybe Text
     -> V.Event
     -> TextZipper -> TextZipper
   interactorEventHandler TextInputConfig{..} pageSize mText = \case
     -- Special characters
     V.EvKey (V.KChar '\t') [] -> complete mText
     V.EvKey (V.KChar  ' ') [] -> complete mText
     ev -> case ev of
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
