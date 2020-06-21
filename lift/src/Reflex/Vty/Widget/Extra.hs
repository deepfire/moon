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
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Zipper
import           Data.Text.Zipper.Extra
import           Data.List                             as List

import           Reflex
import           Reflex.Network
import           Reflex.Vty.Widget                 hiding (text)
import           Reflex.Vty.Widget.Input.RichText
import           Reflex.Vty.Widget.Layout

import qualified Graphics.Vty as V

import           Basis (lcons2, uncurry3)
import           Util (fst3)


type ReflexVty t m =
  ( Adjustable t m
  , MonadFix m
  , MonadHold t m
  , MonadNodeId m
  , NotReady t m
  , PostBuild t m
  , Reflex t
  )

newtype Width  = Width  Int
newtype Height = Height Int
newtype Index  = Index  Int deriving Show
newtype Column = Column Int deriving Show

data Selection a
  = Selection
  { selValue     :: !(Maybe a)
  , selIndex     :: !Index
  , selColumn    :: !Column
  , selInput     :: !Text
  } deriving Show

data MenuInputState t m a b =
  MenuInputState
  { mistSelection :: !(Selection a)
  , mistElems     :: ![a]
  , mistPresent   :: !(a -> Behavior t Bool -> VtyWidget t m a)
  , mistShow      :: !(a -> Text)
  , mistExt       :: !b
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

emptySelection :: Selection a
emptySelection = Selection Nothing (Index 0) (Column 0) ""

selectionUI ::
  (CWidget t m, Eq a, Show a)
  => DynRegion t
  -> (Width -> [a] -> Selection a -> MenuInputState t m a b)
  -> Reflex.Dynamic t [a]
  -> VtyWidget t m (Reflex.Event t (MenuInputState t m a b))
selectionUI region computeMenuState xsD = mdo
  selectionD <- holdDyn emptySelection selectE

  mist0 <-
    computeMenuState
      <$> (Width <$> sample (current $ _dynRegion_width region))
      <*> sample (current xsD)
      <*> sample (current selectionD)

  menuInputStateE <- pure
    $ attachWith lcons2
                 (current $ _dynRegion_width region)
                 (attachWith (,) (current xsD) selectE)
      <&> (fst3 Width >>> uncurry3 computeMenuState)

  selectE :: Event t (Selection a) <-
    menuSelector region mist0 menuInputStateE

  pure menuInputStateE

menuSelector
  :: forall t m a b
  .  ( ReflexVty t m
     , Eq a, Show a)
  => DynRegion t
  -> MenuInputState t m a b
  -> Reflex.Event t (MenuInputState t m a b)
  -> VtyWidget t m (Event t (Selection a))
menuSelector (DynRegion l u w h) mist0 menuStateE = mdo
  selectD    <- holdDyn (mistSelection mist0) selectE

  -- Making this attachWith make input rendering 1 frame stale.
  let internalMenuStateE = attachPromptlyDynWith setMISTSel selectD menuStateE

  selectE    <- switchDyn
                <$> networkHold
                      (frame mist0)
                      (frame <$> internalMenuStateE)
  pure selectE
 where
   frame :: MenuInputState t m a b
         -> VtyWidget t m (Event t (Selection a))
   frame MenuInputState{mistSelection=Selection{..}, ..} = do
     let menuH    = pure $ List.length mistElems + 2
         menuReg  = DynRegion l (u + h - menuH - 1) (w - 2) menuH
         inputReg = DynRegion l (u + h - 1)          w      1

     valueD     <- menu menuReg (fmap snd . focusButton mistPresent)
                     selIndex mistElems
     offtInputE :: Event t (Column, Text) <-
       completingInput inputReg ((mistShow <$>) <$> current valueD)
                       "> " selInput selColumn

     pure $ attachPromptlyDyn valueD offtInputE
       <&> \(val, (newCol, newInput)) ->
             Selection
               { selValue  = val
               , selColumn = newCol
               , selInput  = newInput
               , selIndex  = val <&>
                             (flip List.elemIndex mistElems
                              >>> fmap Index)
                             & join
                             & fromMaybe (Index 0)
               }
   setMISTSel :: Selection a -> MenuInputState t m a b -> MenuInputState t m a b
   setMISTSel sel mist = mist { mistSelection = sel }

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
           textInput completion $ def
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
  void $ child a (current f)
  m <- mouseUp
  k <- key V.KEnter
  return (leftmost [a <$ k, a <$ m], a <$ updated focused)

foregro :: V.Color -> V.Attr
foregro = V.withForeColor V.defAttr
