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
import           Data.Maybe (fromMaybe)
import           Data.Semialign (align)
import           Data.Text (Text, pack, unpack)
import           Data.Text.Zipper
import           Data.Text.Zipper.Extra
import           Data.These
import           Safe

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
data Selector t m a =
  Selector
  { selrInputOfftComplD :: !(Dynamic t (Text, Column, Maybe Char))
  , selrSelectE         :: !(Event t (Index, a))
  }

data SelectorParams t m a =
  SelectorParams
  { sfpElemsE     :: !(Event t [a])
  , sfpCompletep  :: !(Maybe Char -> Char -> a -> Maybe Text)
  , sfpShow       :: !(a -> Text)
  , sfpPresent    :: !(Behavior t Bool -> a -> VtyWidget t m a)
  , sfpInsertW    :: !(VtyWidget t m ())
  }

data Selection a
  = Selection
  { selValue           :: !(Maybe a)
  , selIndex           :: !Index
  , selCompletedByChar :: !(Maybe Char)
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

selector ::
  forall t m a
  . (CWidget t m, Show a)
  => SelectorParams t m a
  -> VtyWidget t m (Selector t m a)
selector SelectorParams{..} = mdo
  lenD <- holdDyn id $ (\l x->x-l) . length <$> sfpElemsE

  -- this is highly suspect!
  selIndexD  :: Dynamic t Index <-
    holdDyn (Index 0) $ fst <$> menuSelE'

  menuChoiceD :: Dynamic t (Maybe (Index, a)) <-
    holdDyn Nothing $ Just <$> menuSelE'

  let menuSelE' = leftmost
        [ menuSelE
        , fmapMaybe id $ sfpElemsE <&> fmap (Index 0,) . flip atMay 0]

  -- This is used immediately above to update the input with menu choice.
  selInputOfftComplD :: Dynamic t (Text, Column, Maybe Char) <-
    holdDyn ("", Column 0, Nothing) $ inputOfftComplE

  -- widgets:  menu + incremental input
  ((_, (menuSelE, menuPickE)
        :: (Event t (Index, a), Event t (Index, a))),
   (_, inputOfftComplE  :: Event t (Text, Column, Maybe Char))) <-
    splitV (pure (\x->x-2)) (pure $ join (,) True)
     (splitV lenD (pure $ join (,) True)
       blank
       (fmap fanEither $
        selectionMenu
          (\(ix, a) ->
             a & focusButton (flip sfpPresent)
               & fmap (fmap (ix,) . fbPress))
          $ attachPromptlyDyn
              selIndexD
              (zip (Index <$> [0..]) <$> sfpElemsE)))
     (splitV (pure $ const 1) (pure $ join (,) True)
       sfpInsertW
       (inputWidget (selInputOfftComplD <&> rpop3) (fmap snd <$> menuChoiceD)
        :: VtyWidget t m (Event t (Text, Column, Maybe Char))))

  pure Selector
    { selrInputOfftComplD = selInputOfftComplD
    , selrSelectE         = menuSelE
    }
 where
   inputWidget ::
        Dynamic t (Text, Column)
     -> Dynamic t (Maybe a)
     -> VtyWidget t m (Event t (Text, Column, Maybe Char))
   inputWidget selInputGuideD menuChoiceD =
     fmap snd <$>
     splitV (pure $ \x->x-1) (pure (False, True))
     (richTextStatic (foregro V.green) $
       maybe "-no data-" sfpShow <$> current menuChoiceD) $
     completingInput
       sfpCompletep (current menuChoiceD)
       "> " selInputGuideD

selectionMenu ::
  forall t m a
  . (Adjustable t m, MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
  => (a -> VtyWidget t m (Event t a))
  -> Event t (Index, [a])
  -> VtyWidget t m (Event t (Either a a))
selectionMenu presentMenuRow xsE =
  fmap (fmap eitherOfTheseL . switchDyn) $
  networkHold
    (staticDataWidget (Index 0) [])
    (xsE <&> uncurry staticDataWidget)
 where
   -- These: This=focus change, That=pick
   staticDataWidget :: Index -> [a] -> VtyWidget t m (Event t (These a a))
   staticDataWidget (Index selIx {- selIx doesn't change! -}) xs
     -- Display elements as row widgets, packaged into 1-large layouts,
     = (zip [0..] xs &) $
         traverse (\(i, x) ->
                     fixed 1 . fmap (fmap (i,)) $ presentMenuRow $ x)
     -- Wrap in an arrow-navigable layout,
     >>> (\itemsLay -> do
             nav :: Event t Int <- upDownNavigation

             focusD :: Dynamic t a
               <-  fmap ((xs !!) . (`mod` length xs)) <$>
                     foldDyn (+) selIx nav
             pickEs <- runLayout (pure Orientation_Column) selIx nav itemsLay
             let pickE = snd <$> leftmost pickEs

             pure $ align (updated focusD) pickE)

completingInput
  :: forall t m a
   . ReflexVty t m
  => (Maybe Char -> Char -> a -> Maybe Text)
     -- ^ Are we completing, given maybe a char to the left,
     --   an element, and the current char -- if yes, then what to?
  -> Behavior t (Maybe a)         -- ^ If user requests completion, that's what to.
  -> Text                         -- ^ Prompt.
  -> Dynamic t (Text, Column)     -- ^ Initial value and position.
  -> VtyWidget t m (Event t (Text, Column, Maybe Char))
completingInput completep completion prompt textColD = do
  width <- displayWidth
  (,) text0 col0 <- sample $ current textColD
  sTxtE :: Event t ((Maybe Char, Text), (Column, Int)) <-
    fmap switchDyn $
    networkHold
    (inputW width (text0, col0))
    . (updated textColD <&>) $
      \(text, col) ->
        inputW width (text, col)
  -- XXX: this dedup might be important!
  --
  -- uSTxtD <- holdUniqDyn sTxtD
  --
    -- ..now.. how does it change our model?
    -- ..as it probably fires coincidentally with txtD
    -- ..should it be an event, or should the TextZipper be just extended?
  pure $ sTxtE <&>
    \((mc, t), (col, _)) -> (t, col, mc)
 where
   inputW ::
        Dynamic t Int
     -> (Text, Column)
     -> VtyWidget t m (Event t ((Maybe Char, Text), (Column, Int)))
   inputW width (text0, Column col0) =
     row $ do
       fixedInert 2 $
         richTextStatic (foregro V.blue) (pure prompt)
       fixed (width - 2) $
         fmap (\(TextInput sTxtD _ xyD) ->
                 updated $ zipDyn sTxtD (first Column <$> xyD)) $
         textInput completion $
           (defaultTextInputConfig Nothing)
           { _textInputConfig_initialValue =
               fromText text0 & top & rightN col0 & (Nothing,)
           , _textInputConfig_handler = interactorEventHandler
           }
   interactorEventHandler
     :: TextInputConfig t (Maybe Char) a
     -> Int
     -> Maybe a
     -> V.Event
     -> TextZipper -> (Maybe Char, TextZipper)
   interactorEventHandler TextInputConfig{..} pageSize completeTo = \case
     -- Ignore Tab chars, if they leak through.
     V.EvKey (V.KChar '\t') [] -> (Nothing,)
     -- Special characters
     V.EvKey (V.KChar c) [] ->
       \z -> case join $ maybe Nothing (Just . completep (lookCharLeft z) c) completeTo of
               Just complTo -> (Just c, complete c (Just complTo) z)
               Nothing      -> (Nothing, insertChar c z)
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

instance (Show a) => Show (Selection a) where
  show Selection{..} = unpack $ mconcat
    [ "#<Sel "
    , "i", pack . show $ unIndex selIndex, " "
    , "v:", pack $ show selValue, " "
    -- , "e:", pack $ show selExt
    , ">"
    ]

emptySelection :: Selection a
emptySelection = Selection Nothing (Index 0) Nothing
