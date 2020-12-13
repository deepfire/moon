{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Vty.Widget.Selector (module Reflex.Vty.Widget.Selector) where

import Safe

import Control.Monad
import Control.Monad.Fix
import Control.Monad.NodeId

import Data.Function
import Data.Semialign                               (align)
import Data.Sequence qualified                    as Seq
import Data.Text                        qualified as T
import Data.Text.Zipper
import Data.Text.Zipper.Extra
import Data.These
import Data.Vector qualified                      as Vec
import Data.Vector                                  (Vector)

import Reflex
import Reflex.Network
import Reflex.Vty.Widget                     hiding (text)
import Reflex.Vty.Widget.Input.RichText
import Reflex.Vty.Widget.Layout

import Graphics.Vty                     qualified as V

import Basis                                 hiding (Dynamic, left, right)

import Reflex.Vty.Widget.Extra

import Debug.Reflex


--------------------------------------------------------------------------------
-- * Selector:  completing input + menu
--
data Selector t m a =
  Selector
  { selrInputOfftD :: !(Dynamic t (Text, Column))
  , selrSelectE    :: !(Event t (Index, a))
  }

data SelectorParams t m a =
  SelectorParams
  { spElemsE       :: !(Event t [a])
  , spCompletep    :: !(Maybe Char -> Char -> a -> Maybe Text)
  , spShow         :: !(a -> Text)
  , spPresent      :: !(Behavior t Bool -> a -> VtyWidget t m a)
  , spInsertW      :: !(VtyWidget t m ())
  , spConstituency :: !(Char -> Bool)
  }

type CWidget t m = (Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)

selector ::
  forall t m a
  . (CWidget t m, Show a)
  => SelectorParams t m a
  -> VtyWidget t m (Selector t m a)
selector SelectorParams{..} = mdo
  lenD <- holdUniqDyn =<< holdDyn 0 ($(ev "lenD" 'spElemsE) <&> length)

  -- this is highly suspect!
  selIndexD  :: Dynamic t Index <-
    holdDyn (Index 0) $ $(ev "selIndexD" 'selrSelectE) <&> fst

  menuChoiceD :: Dynamic t (Maybe (Index, a)) <-
    holdDyn Nothing $(ev "menuChoiceD" 'maySelrSelectE)

  let selrSelectE = fmapMaybe id $(ev "selrSelectE" 'maySelrSelectE)

      maySelrSelectE =
        leftmost
        [ $(ev' "maySelrSelectE" "selectionMenu") menuMaySelE
        -- The initial value:
        , $(ev "maySelrSelectE" 'spElemsE)
          <&> fmap (Index 0,) . flip atMay 0]

  -- This is used immediately above to update the input with menu choice.
  selrInputOfftD :: Dynamic t (Text, Column) <-
    holdDyn ("", Column 0) $
    $(evl "selrInputOfftD" 'inputOfftErrE
          [e|showQ . unpack . fst3|]) <&> rpop3

  -- widgets:  menu + incremental input
  ((_, (menuMaySelE,  _menuPickE)
        :: ( Event t (Maybe (Index, a))
           , Event t (Index, a))),
   (_, inputOfftErrE :: Event t (Text, Column, Maybe Text))) <-
    splitV (pure (\x->x-2)) (pure $ join (,) True)
     (splitV (lenD <&> \x y -> y - x) (pure $ join (,) True)
       blank
       (fmap fanEither $
        selectionMenu
          (\(ix, a) ->
             a & focusButton (flip spPresent)
               & fmap (fmap (ix,) . fbPress))
          $ attachPromptlyDyn
              $(dev "indexXsE" 'selIndexD)
              ($(evl "indexXsE" 'spElemsE [e|show . length|])
                <&> zip (Index <$> [0..]))))
     (splitV (pure $ const 1) (pure $ join (,) True)
       spInsertW
       (inputWidget
          $(devl  "selInputGuideD" 'selrInputOfftD
                  [e|showQ . fst|])
          (current menuChoiceD
           <&> fmap snd)
        :: VtyWidget t m (Event t (Text, Column, Maybe Text))))

  pure Selector{..}
 where
   inputWidget ::
        Dynamic t (Text, Column)
     -> Behavior t (Maybe a)
     -> VtyWidget t m (Event t (Text, Column, Maybe Text))
   inputWidget selInputGuideD menuChoiceB =
     fmap snd <$>
     splitV (pure $ \x->x-1) (pure (False, True))
     (richTextStatic (foregro V.green) $
       maybe "-no data-" spShow <$> menuChoiceB) $
     completingInput
       spConstituency
       spCompletep
       menuChoiceB
       "> "
       $(devl "inputOfftErrE" 'selInputGuideD
              [e|showQ . fst|])

selectionMenu ::
  forall t m a
  . (Adjustable t m, MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t, HasCallStack)
  => (a -> VtyWidget t m (Event t a))
  -> Event t (Index, [a])
  -> VtyWidget t m (Event t (Either (Maybe a) a))
selectionMenu presentMenuRow indexXsE = mdo
  vportHeightD <- fmap Height <$> displayHeight
  height0 <- sample $ current vportHeightD
  let initial = (0, Index 0, Vec.empty)
      accumVportState ::
           Either (Int, Index) (Index, Vector (Int, a))
        -> (Int, Index, Vector (Int, a))
        -> (Int, Index, Vector (Int, a))
      accumVportState (Left  (x, y)) u@(_, _, z) = (x, y, z)
        & trace ("accumVportState: L "<>show (Vec.length <$> u)<>
                 " -> " <> show (Vec.length <$> (x, y, z)))
      accumVportState (Right (y, z)) u@(x, _, _) = (x, y, z)
        & trace ("accumVportState: R "<>show (Vec.length <$> u)<>
                 " -> " <> show (Vec.length <$> (x, y, z)))
  vportStateD <- foldDyn accumVportState initial $
    leftmost [ Left <$> panE
             , Right <$> $(evl'' "staticDataWidget_postBuildE"
                              "indexXsE"
                              [e|show . length . snd|])
                         (fmap (Vec.indexed . Vec.fromList) <$> indexXsE)
             ]
  (,) (panE :: Event t (Int, Index))
      (selE :: Event t (Either (Maybe a) a))
    <- fmap (fanEither . fmap (fmap eitherOfTheseL) . switchDyn) $
         networkHold
           (staticDataWidget height0 initial)
           (uncurry staticDataWidget
            <$> attachPromptlyDyn
                  vportHeightD
                  (updated vportStateD))
  pure selE
 where
   -- These: This=focus change, That=pick
   staticDataWidget :: HasCallStack =>
        Height
     -> (Int, Index, Vector (Int, a))
     -> VtyWidget t m (Event t (Either (Int, Index) (These (Maybe a) a)))
   staticDataWidget _ (_, _, Vec.null -> True)
     = pure never
     -- If there's nothing to complete _to_, signal this:
     -- = getPostBuild
     --   <&> $(evl'' "menuMaySelE" "staticDataWidget_postBuildE"
     --               [e|const "Nothing"|])
     --       . ($> This Nothing)
   staticDataWidget (Height height) (vportPos, Index selIx, vec) = do
     -- Display elements as row widgets, packaged into 1-large layouts,
     let len     = Vec.length vec
         visible = (len - vportPos) `min` height
         slice   = Vec.slice vportPos visible vec
         sliceIx = selIx - vportPos
         initial = snd $ slice Vec.! sliceIx

     let layout = trace ("traversing slice "<>show vportPos<>":"<>show visible<>":"<>show (vportPos + visible)<>"/"<>show len<>", height="<>show height) $
                  flip traverse slice $
                  \(i, x) ->
                    fixed 1 . fmap (fmap (i,)) . presentMenuRow $ x

     upDownNav :: Event t Int <- upDownNavigation

     focusIx <- foldDyn (+) selIx $(ev "focusIx" 'upDownNav)
                  <&> fmap ((\ix->trace ("new focusIx: "<>show ix) ix) .
                            (`mod` len))

     let panE = fmapMaybe id $
                updated focusIx <&> focusChangeNecessitatesPan
         focusChangeNecessitatesPan :: Int -> Maybe (Int, Index)
         focusChangeNecessitatesPan ix =
           (\x->trace ("pan: "<>show ix<>" (of "<>show len<>", vport "<>show vportPos<>"-"<>show (vportPos + height)<>") -> "<>show x) x) $
           (,Index ix) <$>
           if | ix < vportPos
                -> let wrap = ix == 0
                   in Just if wrap then 0
                           else max 0 (ix - height + 2)
              | ix >= vportPos + height
                -> let wrap = ix == len - 1
                   in Just if wrap then len - height + 1
                           else min (len - (height - 2)) (ix - 2)
              | otherwise
                -> Nothing

     let focusE :: Event t a = snd . (vec Vec.!) <$> updated focusIx

     -- pickEs <- runLayout (pure Orientation_Column)
     void $ runLayout (pure Orientation_Column)
                      sliceIx
                      $(ev "pickE" 'upDownNav)
                      layout
     -- let pickE = snd <$> leftmost pickEs

     nowE <- getPostBuild
     pure $
       leftmost
         [ fmap Left panE
         , fmap Right $ align
             (leftmost
              [ $(evl'' "selectionMenu" "staticDataWidget_postBuildE"
                   [e|const "Just"|]) nowE
                <&> const initial
              , $(ev "selectionMenu" 'focusE)
              ]
              <&> Just)
             never
         -- $(ev "selectionMenu" 'pickE)
         ]

completingInput ::
  forall t m a
   . ReflexVty t m
  => (Char -> Bool)
  -> (Maybe Char -> Char -> a -> Maybe Text)
     -- ^ Are we completing, given maybe a char to the left,
     --   an element, and the current char -- if yes, then what to?
  -> Behavior t (Maybe a)         -- ^ If user requests completion, that's what to.
  -> Text                         -- ^ Prompt.
  -> Dynamic t (Text, Column)     -- ^ Initial value and position.
  -> VtyWidget t m (Event t (Text, Column, Maybe Text))
completingInput constituency completep completion prompt textColD = do
  width <- displayWidth
  (,) text0 col0 <- sample $ current textColD
  sTxtE :: Event t (Text, (Column, Int), Maybe Text) <-
    fmap switchDyn $
    networkHold
    (inputW width (text0, col0))
    . (updated textColD <&>) $
      \(text, coln) ->
        inputW width (text, coln)
  -- XXX: this dedup might be important!
  --
  -- uSTxtD <- holdUniqDyn sTxtD
  --
    -- ..now.. how does it change our model?
    -- ..as it probably fires coincidentally with txtD
    -- ..should it be an event, or should the TextZipper be just extended?
  pure $ sTxtE <&>
    \(t, (coln, _), mErr) -> (t, coln, mErr)
 where
   inputW ::
        Dynamic t Int
     -> (Text, Column)
     -> VtyWidget t m (Event t (Text, (Column, Int), Maybe Text))
   inputW width (text0, Column col0) =
     row $ do
       fixedInert 2 $
         richTextStatic (foregro V.blue) (pure prompt)
       join $ fixed (width - 2) $
         fmap (\(TextInput sTxtD _ xyD) -> do
                 let inputD = zipDynWith
                              (\(mErr, txt) (col, row) ->
                                 (txt, (Column col, row), mErr))
                              sTxtD xyD
                 uniqInputD <- holdUniqDyn inputD
                 pure $ updated uniqInputD) $
         textInput completion $
           (defaultTextInputConfig Nothing)
           { _textInputConfig_initialValue =
               fromText text0 & top & rightN col0 & (Nothing,)
           , _textInputConfig_handler = interactorEventHandler
           }
   interactorEventHandler
     :: TextInputConfig t (Maybe Text) a
     -> Int
     -> Maybe a
     -> V.Event
     -> TextZipper -> (Maybe Text, TextZipper)
   interactorEventHandler TextInputConfig{} pageSize completeTo vtyEv =
     -- Note:  all branches are :: TextZipper -> TextZipper
     case vtyEv of
       -- Ignore Tab chars, if they leak through:
       V.EvKey (V.KChar '\t') [] -> (Nothing,) . id
       -- Regular characters, no modifiers:
       V.EvKey (V.KChar c) [] ->
         \z ->
           case join $ completep (lookCharLeft z) c <$> completeTo of
             Just complTo ->
               (,) Nothing $
               (\x@(TextZipper _ res _ _)-> flip trace x . mconcat $
                 [ "completed to _", unpack res, "_, "
                 , "from char '", [c], "'"
                 ]) $
               complete constituency c (Just complTo) z
             Nothing -> case c of
               '(' -> handleParenOpen z
               ')' -> handleParenClose z
               '"' -> handleQuote z
               _ -> (,) Nothing $ insertChar c z
       -- Deletion buttons
       V.EvKey V.KBS  [] -> handleDeleteLeft
       V.EvKey V.KDel [] ->
         -- 'Del' key is a plain, non-fancy escape hatch.
         (Nothing,) . deleteRight
       ev -> (Nothing,) .
         case ev of
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
    where
      handleParenOpen, handleParenClose ::
        TextZipper -> (Maybe Text, TextZipper)
      handleParenOpen (TextZipper lb b a la) =
        (,) Nothing
            -- Introduce a balanced pair:
            (TextZipper lb (b `T.snoc` '(')
                           (')' `T.cons` a) la)
      handleParenClose z@(TextZipper lb b a la) =
        -- Find if the right side has the closing paren:
        case T.uncons <$> T.span (/= ')') a of
          (insideParens, Just (')', rest)) ->
            (,) Nothing
                -- Pretend the closing paren was always there..
                (TextZipper lb (b <> insideParens `T.snoc` ')')
                               rest la)
          _ -> (,) (Just "Not inside a list.") z
      handleDeleteLeft ::
        TextZipper -> (Maybe Text, TextZipper)
      handleDeleteLeft z@(TextZipper lb b a la) =
        case (T.unsnoc b, T.uncons a) of
          (Just (_, ')'), _) ->
            (,) Nothing
                (left z)
          (Just (b', '('), Just (c, a')) ->
            case c of
              ')' -> (,) Nothing
                         -- Annihilate the balanced pair:
                         (TextZipper lb b' a' la)
              _ -> (,) (Just "Beginning of a list!")
                       z
          (Just (b', '"'), r) ->
            case r of
              Just ('"', a') ->
                   (,) Nothing
                   -- Annihilate the balanced pair:
                       (TextZipper lb b' a' la)
              _ -> (,) Nothing
                       (left z)
          _ -> (,) Nothing
                   (deleteLeft z)
      handleQuote ::
        TextZipper -> (Maybe Text, TextZipper)
      handleQuote (TextZipper lb b a la) =
        case (T.unsnoc b, T.uncons a) of
          (Just (_, '"'), Just ('"', a')) ->
            (,) Nothing
                -- Pretend the closing quote was always there..
                (TextZipper lb (b `T.snoc` '"')
                               a' la)
          (mc, _) ->
            (,) Nothing
                (TextZipper lb (b <> if ((== ' ') . snd <$> mc) == Just False
                                     then " \"" else "\"")
                               ('"' `T.cons` a) la)
