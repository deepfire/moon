{-# LANGUAGE NoMonomorphismRestriction #-}
module CLI (cli) where

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
import           Data.Map (Map)
import           Data.Maybe
import           Data.Maybe
import           Data.Text
import           Data.Text (Text)
import           Options.Applicative               hiding (switch)
import           Options.Applicative.Common
import           Reflex                            hiding (Request)
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty                        hiding (Request)
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

import Basis hiding (Dynamic)
import Ground.Hask
import Lift
import Pipe hiding (Dynamic)
import Wire.Peer
import Wire.Protocol

import qualified System.IO.Unsafe                       as Unsafe


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
           interact' space
       ) of
       Right x -> x
       Left  e -> fail (unpack e)
     pure ClientDone

type ReflexVty t m = (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, NotReady t m, PostBuild t m, MonadNodeId m)

data Hints = Hints
  { unHints :: [SomeDesc]
  }

data HintsOutput t = HintsOutput
  { hoHints   :: Dynamic t Hints
  -- , hoCurrent ::
  }

interact'
  :: PipeSpace (SomePipe ()) -> IO ()
interact' s = mainWidget $ mdo
  inp <- input
  (,) w h <- (,) <$> displayWidth <*> displayHeight

  descsD <- holdDyn descs never
  descFilterD <- holdDyn (const True) $ updated filtD
  descsFiltereD <- pure $ zipDynWith (\ds f-> Prelude.filter f ds) descsD descFilterD
  msdD <- pane (menuR w) (pure True) $ col $ do
    sdE <- fixed (const 15 . Prelude.length <$> descsFiltereD) $
      selectSomeDesc (menuR w) descsFiltereD
    -- selsD <- holdDyn "" $ showName . someDescName <$> traceEvent "sdE" sdE
    -- fixedInert 1 $ text (("sel=" <>) <$> current selsD)
    -- msdD <- holdDyn (listToMaybe descs) (Just <$> sdE)
    msdD <- holdDyn Nothing (Just <$> sdE)
    pure $ msdD
  filtD <- pane (inputR w) (pure True) $ col $ mdo
    let mexp = parseLocated <$> i
        err = fromMaybe "" . eitherLeft  <$> mexp -- Dynamic Text
        exp = eitherRight <$> mexp -- Dynamic (Expr (SourcePos, QName Pipe, SourcePos))
    i <- fmap _textInput_value . fixed 1 . row $ do
      fixedInert 2 $ richTextStatic (V.withForeColor V.defAttr V.blue) "> "
      stretch $ textInput def
    fixedInert 3 $ richTextStatic (V.withForeColor V.defAttr V.red) (current err)
    pure $ (\i -> (\(someDescName -> n) -> T.isInfixOf i $ showName n)) <$> i

  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
 where
   ndescs = Prelude.length descs
   menuR, inputR :: Reflex t => Dynamic t Int -> DynRegion t
   menuR  w = DynRegion 0 0 w (pure $ 15)
   inputR w = DynRegion 0 (pure 15) w 4
   descs  = sortBy (compare `on` someDescName) $
            somePipeDesc <$> psfrom s
   psfrom = pipesFrom (SomeTypeRep $ typeRep @())
   screen = DynRegion 0 0
   escapable w = do
     void w
     i <- input
     return $ fforMaybe i $ \case
       V.EvKey V.KEsc [] -> Just $ Nothing
       _ -> Nothing

selectSomeDesc
  :: forall t m a
  . ( Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t
    , a ~ ())
  => DynRegion t
  -> Dynamic t [SomeDesc] -- KEY -- this only triggers change if it is changed
  -> VtyWidget t m (Event t a)
selectSomeDesc r xsD = mdo
  xs0 <- sample $ current xsD
  -- networkHold buttons $ ffor (switch (current out))
  -- networkHold (menu xs0) $ fmap menu (updated xsD)
  rec out :: Dynamic t (Event t a)
        <- networkHold (menu xs0) $ ffor (switch (pure $ updated xsD))
           (menu
            :: [SomeDesc] -> VtyWidget t m (Event t a))

  pure $ switchDyn out
 where
   menu :: [SomeDesc]
        -> VtyWidget t m (Event t a)
   menu xs =
     leftmost <$>
     pane r (pure True)
     (col $ do
       fixedInert 1 $ text "woot" --debugFocus -- self-defeating, huh?
       fixedInert 1 $ text "yay" --debugFocus -- self-defeating, huh?
       -- why does this obscure the entries below??
       traverse (fixed 3 . textButtonStatic def . pack . show)
         xs)
       -- traverse (fixed 1 . focusButton (someDescSelectable nameWi sigWi))
       --   xs)
    where nameWi = Prelude.maximum $ T.length . showName . someDescName <$> xs
          sigWi  = Prelude.maximum $ T.length . showSig  . someDescSig  <$> xs

someDescSelectable
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t)
  => Int
  -> Int
  -> SomeDesc
  -> Behavior t Bool
  -> VtyWidget t m ()
someDescSelectable nameWi sigWi (SomeDesc pd) focusB = row $ do
  fixed 7 $ debugFocus
  fixed (pure nameWi) $
    richText (textFocusStyle (V.withForeColor V.defAttr V.green) focusB)
      (pure . justifyRight nameWi ' ' . showName $ pdName pd)
  fixed 4 $
    richText (textFocusStyle V.defAttr focusB)
      (pure " :: ")
  fixed (pure sigWi) $
    richText (textFocusStyle (V.withForeColor V.defAttr V.blue) focusB)
      (pure . justifyLeft   sigWi ' ' . showSig $ pdSig pd)
 where
   textFocusStyle attr focusB =
     (RichTextConfig $ selecting
       (flip V.withBackColor $ V.rgbColor 1 1 1)
       (pure attr)
       focusB)

richTextStatic = richText . RichTextConfig . pure

focusButton
  :: (Reflex t, Monad m, MonadNodeId m)
  => (a -> Behavior t Bool -> VtyWidget t m ())
  -> a
  -> VtyWidget t m (Event t a)
focusButton child a = do
  f <- focus
  child a (current f)
  m <- mouseUp
  k <- key V.KEnter
  return $ leftmost [a <$ k, a <$ m]

col'
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => VtyWidget t m (Event t Int)
  -> Layout t m a
  -> VtyWidget t m a
col' navE child = do
  nav <- navE
  runLayout (pure Orientation_Column) 0 nav child

fixedInert
  :: (Reflex t, Monad m, MonadNodeId m)
  => Dynamic t Int
  -> VtyWidget t m a
  -> Layout t m a
fixedInert sz = tile (TileConfig
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

testBoxes
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => Text -> VtyWidget t m ()
testBoxes initial = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = DynRegion (div' dw 6) (div' dh 6) (div' dw 2) (div' dh 2)
      region2 = DynRegion (div' dw 4) (div' dh 4) (2 * div' dw 3) (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  _ <- pane region2 (constDyn True) . boxStatic singleBoxStyle $
    let cfg = def
          { _textInputConfig_initialValue = TZ.fromText initial }
        textBox = boxStatic roundedBoxStyle $ multilineTextInput cfg
        dragBox = boxStatic roundedBoxStyle dragTest
    in splitVDrag (hRule doubleBoxStyle) textBox dragBox
  return ()
  where
    div' :: (Integral a, Applicative f) => f a -> f a -> f a
    div' = liftA2 div

debugInput :: (Reflex t, MonadHold t m) => VtyWidget t m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent
debugFocus :: (Reflex t, Monad m) => VtyWidget t m ()
debugFocus = do
  f <- focus
  text $ T.pack . show <$> current f
dragTest :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

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
