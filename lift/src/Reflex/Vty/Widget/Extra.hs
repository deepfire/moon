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

data Selection a
  = Selection
  { sValue     :: !(Maybe a)
  , sColumn    :: !Int
  , sInput     :: !Text
  } deriving Show

newtype Width  = Width Int
newtype Height = Height Int

data MenuInputState t m a b =
  MenuInputState
  { misSelection :: !(Selection a)
  , misElems     :: ![a]
  , misPresent   :: !(a -> Behavior t Bool -> VtyWidget t m a)
  , misShow      :: !(a -> Text)
  , misExt       :: !b
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

selectionUI ::
  (CWidget t m, Eq a, Show a)
  => DynRegion t
  -> (Width -> [a] -> Selection a -> MenuInputState t m a b)
  -> Reflex.Dynamic t [a]
  -> VtyWidget t m (Reflex.Dynamic t (MenuInputState t m a b))
selectionUI region computeMenuState xsD = mdo
  selectionD <- holdDyn (Selection Nothing 0 "") selectionE

  menuStateD <- pure
    $ zipDynWith lcons2 (_dynRegion_width region) (zipDynWith (,) xsD selectionD)
      <&> (fst3 Width >>> uncurry3 computeMenuState)

  selectionE :: Event t (Selection a) <-
    menuSelector region menuStateD

  pure menuStateD

menuSelector
  :: forall t m a b
  .  ( ReflexVty t m
     , Eq a, Show a)
  => DynRegion t
  -> Reflex.Dynamic t (MenuInputState t m a b)
  -> VtyWidget t m (Event t (Selection a))
menuSelector (DynRegion l u w h) menuStateD = mdo
  text0      <- pure ""
  selectionD <- holdDyn (Selection Nothing 0 text0) inputE
  let feeD  = zipDynWith (\ms@MenuInputState{misElems}
                           (Selection mEntry inputCol inputText) ->
                            ( ms
                            , inputText
                            , fromMaybe 0 $ join $ mEntry <&>
                                                   flip List.elemIndex misElems
                            , inputCol))
              menuStateD selectionD
  (ms0, _text0, sel0, pos0)
           <- sample (current feeD)
  inputE   <- switchDyn <$>
              networkHold (frame ms0 text0 sel0 pos0)
              (updated $
               feeD <&> \(ms, text, sel, pos) ->
                           frame ms  text  sel  pos)
  pure inputE
 where
   frame :: MenuInputState t m a b
         -> Text
         -> Int
         -> Int
         -> VtyWidget t m (Event t (Selection a))
   frame MenuInputState{..} text selIx pos = do
     let menuH    = pure $ List.length misElems + 2
         menuReg  = DynRegion l (u + h - menuH - 1) (w - 2) menuH
         inputReg = DynRegion l (u + h - 1)          w      1

     selD       <- menu menuReg (fmap snd . focusButton misPresent)
                     selIx misElems
     offtInputE <- completingInput inputReg
                     ((misShow <$>) <$> current selD)
                     "> " text pos

     pure $ attachPromptlyDyn selD offtInputE
       <&> \(a, (b, c)) -> Selection a b c

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
