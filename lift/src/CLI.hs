{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# OPTIONS_GHC -Wall #-}

module CLI (cli) where

import           Safe

import           Codec.Serialise                          (Serialise)
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Control.Tracer
                 (Tracer, stdoutTracer, showTracing, traceWith)
import qualified Control.Zipper                         as Zip
import           Data.Functor.Misc
import qualified Data.IntervalMap.FingerTree            as IMap
import qualified Data.List                              as List
import           Data.Map (Map)
import           Data.Maybe
import           Data.Maybe
import           Data.Text
import           Data.Text (Text)
import           Data.Tuple.All
import           Options.Applicative               hiding (switch)
import           Options.Applicative.Common
import           Reflex                            hiding (Request)
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty                        hiding (Request)
import           Reflex.Vty.Widget.Layout
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import qualified Data.Text.Zipper                       as TZ
import qualified Graphics.Vty                           as V
import qualified Network.WebSockets                     as WS
-- import qualified Control.Concurrent.STM.TMVar           as TM
-- import qualified Control.Concurrent.STM.TBQueue         as TQ

import           Text.Parsec.Pos (SourcePos)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Core             as Net
import qualified Network.TypedProtocol.Driver           as Net

import qualified Reflex.Vty.Widget.Input.RichText       as Rich

import Basis hiding (Dynamic)
import Ground.Hask
import Lift
import Pipe hiding (Dynamic)
import Wire.Peer
import Wire.Protocol
import Debug.Reflex

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f <$$> xs = (f <$>) <$> xs

(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)

rcons :: (a, b) -> c -> (a, b, c)
rcons (a, b) c = (a, b, c)

lcons :: a -> (b, c) -> (a, b, c)
lcons a (b, c) = (a, b, c)

luncons :: (a, b, c) -> (a, (b, c))
luncons (a, b, c) = (a, (b, c))

rpop :: (a, b, c) -> (a, b)
rpop (a, b, _) = (a, b)

menu :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
        => DynRegion t
        -> (a -> VtyWidget t m (Event t a, Event t a))
        -> Int
        -> [a]
        -> VtyWidget t m (Event t a, Event t a)
menu region displayMenuRow initSel =
  fmap (join (***) leftmost . unzip)
  . pane region (constDyn True)
  . boxStatic roundedBoxStyle
  . (\child -> do
        nav <- upDownNavigation
        runLayout (pure Orientation_Column) initSel nav child)
  . traverse (fixed 1 . displayMenuRow)

data SelectorFrame a
  = Frame
    { sfSelection :: !a
    , sfColumn    :: !Int
    , sfText      :: !Text
    }

foregro :: V.Color -> V.Attr
foregro = V.withForeColor V.defAttr

menuSelector
  :: forall t m a b
  .  ( ReflexVty t m
     , Eq a, Show a)
  => DynRegion t
  -> (Dynamic t (Int, Text) -> Dynamic t (b, [a], Text))
  -> (b -> a -> Behavior t Bool -> VtyWidget t m a)
  -> VtyWidget t m (Event t a)
menuSelector r xsDF present = mdo
  text0    <- pure ""
  inputD   <- holdDyn (Nothing, (0, text0)) (luncons <$> inputE)
  let xsDr  = xsDF (snd <$> inputD)
      xsD   = zipDynWith (\(ctx, xs, trc) mx ->
                            ( fromMaybe 0 $ join $ flip List.elemIndex xs <$> mx
                            , ctx
                            , xs
                            , trc))
              xsDr (fst <$> inputD)
  (sel0, ctx0, xs0, trc0)
           <- sample (current $ xsD)
  frameDE  <- networkHold  (frame r (present ctx0) text0 xs0 sel0 trc0)
              (updated $ zipDyn (snd . snd <$> inputD) xsD <&>
                          \(text, (sel, ctx, xs, trc)) ->
                            frame r (present ctx)  text  xs  sel  trc)
  let (inputE, xE) = fanEither (switchDyn frameDE)

  pure xE
 where
   frame :: DynRegion t -> (a -> Behavior t Bool -> VtyWidget t m a)
         -> Text -> [a] -> Int -> Text
         -> VtyWidget t m (Event t (Either (Maybe a, Int, Text) a))
   frame r@(DynRegion l u w h) presentMenuRow text xs sel trc = do
     let menuH      = pure $ List.length xs + 2
         menuRegion = DynRegion l (u + h - menuH - 4) (zipDynWith min (w - 2) 50) menuH
         -- ruleRegion = DynRegion l (u + h - 5)         w       1
         intrRegion = DynRegion l (u + h - 4)         w       1
         erroRegion = DynRegion l (u + h - 3)         w       3
     (valE, selE)
       <- menu menuRegion (focusButton presentMenuRow) sel xs
     selD <- holdDyn (xs `atMay` sel) (Just <$> selE)
     -- pane ruleRegion (pure False) $
     --   hRule roundedBoxStyle
     xFltE <- (Left . uncurry lcons <$>) . attachPromptlyDyn selD
              <$> interactor intrRegion text
     pane erroRegion (pure False) $
       richTextStatic (foregro V.red) (pure trc)
     pure $ leftmost [Right <$> valE, xFltE]
   interactor
     :: (MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m, Reflex t)
     => DynRegion t -> Text -> VtyWidget t m (Event t (Int, Text))
   interactor r@(DynRegion _ _ rw _) initial = do
     Rich.TextInput txtD _ xyD <-
       pane r (pure True) $ row $ do
         fixedInert 2 $
           richTextStatic (foregro V.blue) "> "
         fixed (rw - 2) $
           Rich.textInput $ def
           { Rich._textInputConfig_initialValue = TZ.fromText initial }
     utxtD <- holdUniqDyn txtD
     pure $ attachPromptlyDyn (fst <$> xyD) (updated utxtD)

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


cli :: IO ()
cli = do
  mreq <- execParser $ (info $ (optional parseRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req    = fromMaybe (Run "meta.space") mreq
          chan   = channelFromWebsocket conn
      runClient tracer (client tracer req) $ channelFromWebsocket conn

client :: forall rej m a
          . (rej ~ Text, m ~ IO, a ~ Reply)
       => Tracer m String
       -> Request
       -> m (ClientState rej m a)
client tracer req = pure $
  ClientRequesting req handleReply
 where
   handleReply (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure ClientDone
   handleReply (Right (ReplyValue rep)) = do
     putStrLn $ show rep
     case
       withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) rep
       (\(VPoint space) -> do
           traceWith tracer $ "Got pipes.."
           mainWidget $ interact' space
       ) of
       Right x -> x
       Left  e -> fail (unpack e)
     pure ClientDone

type ReflexVty t m = (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, NotReady t m, PostBuild t m, MonadNodeId m)

interact'
  :: ReflexVty t m
  => PipeSpace (SomePipe ()) -> VtyWidget t m (Event t ())
interact' s = do
  inp <- input
  w   <- displayWidth
  h   <- displayHeight

  sdE <- menuSelector
         (DynRegion 0 0 w (h - 1))
         (fmap $ \(col, input) ->
             case parseLocated input of
               Left _e  -> ((3, 3), [], "")
               Right exp@(indexLocated -> index) ->
                 let name = case lookupLocatedQName col index of
                              Nothing -> ""
                              Just qn -> showQName qn
                     xs = infixNameFilter name `Prelude.filter` descs
                  in ( presentCtx xs
                     , xs
                     , name))
         someDescSelectable
  sdD <- holdDyn Nothing (Just <$> sdE)

  pane (DynRegion 0 (h - 1) w 1) (pure False) $
    richText (RichTextConfig $ pure (foregro V.green))
    (T.pack . fromMaybe "<none>" . fmap show <$> current sdD)

  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
 where
   presentCtx = (((showName . someDescName <$>)
                   &&&
                  (showSig  . someDescSig  <$>))
                  >>>
                 (join (***) (Prelude.maximum . (T.length <$>))))
   infixNameFilter text = isInfixOf text . showName . someDescName
   ndescs = Prelude.length descs
   descs  = sortBy (compare `on` someDescName) $
            somePipeDesc <$> psfrom s
   psfrom = pipesFrom (SomeTypeRep $ typeRep @())
   escapable w = do
     void w
     i <- input
     return $ fforMaybe i $ \case
       V.EvKey V.KEsc [] -> Just $ Nothing
       _ -> Nothing
   someDescSelectable
     :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t)
     => (Int, Int)
     -> SomeDesc
     -> Behavior t Bool
     -> VtyWidget t m SomeDesc
   someDescSelectable (nameWi, sigWi) sd@(SomeDesc pd) focusB = row $ do
     fixed (pure nameWi) $
       richText (textFocusStyle (foregro V.green) focusB)
         (pure $ justifyRight nameWi ' ' . showName $ pdName pd)
     fixed 4 $
       richText (textFocusStyle V.defAttr focusB)
         (pure " :: ")
     fixed (pure sigWi) $
       richText (textFocusStyle (foregro V.blue) focusB)
         (pure $ justifyLeft  sigWi ' ' . showSig $ pdSig pd)
     pure sd
    where
      textFocusStyle attr focB =
        RichTextConfig $
          selecting (flip V.withBackColor $ V.rgbColor 1 1 1)
                    (pure attr)
                    focB

   -- look :: PipeSpace (SomePipe ()) -> QName Pipe -> Either Text (SomePipe ())
   -- look space name = lookupSpace name space &
   --   guard ("No such pipe: " <> showQName name)
   -- loop :: Cons.InputT IO ()
   -- loop = do
   --   minput <- Cons.getInputLine "> "
   --   case minput of
   --     Nothing -> pure ()
   --     Just "quit" -> pure ()
   --     Just input -> do
   --       Cons.outputStrLn $ "Input was: " ++ input
   --       case parse (pack input) of
   --         Left e -> Cons.outputStrLn $ "Parse:  " ++ unpack e
   --         Right exp ->
   --           case compil e opsDesc (pure . look s) exp of
   --             Left e -> Cons.outputStrLn $ "Compile:  " ++ unpack e
   --             Right pipe -> Cons.outputStrLn $ "OK:  " <> show pipe
   --       loop
   -- case (,) (lookupSpace "meta.fromrep" space)
   --            (lookupSpace "meta.unit" space)
   --     of (,) (Just fromrep)
   --            (Just unit) -> do
   --          case apply (app opsDesc) fromrep
   --               (SomeValue
   --                (SomeKindValue
   --                 TPoint (VPoint (Name "Unit" :: Name Type)))) of
   --            Left e -> putStrLn (unpack e)
   --            Right p -> putStrLn (show p)
