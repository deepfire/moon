--{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

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
import           Data.Text (Text)
import           Data.Tuple.All
import qualified Type.Reflection                        as R

import           Options.Applicative               hiding (switch)
import           Options.Applicative.Common
import           Reflex                            hiding (Request)
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty                        hiding (Request)
import           Reflex.Vty.Widget.Layout
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Char                              as C
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import qualified Data.Text.Zipper                       as TZ
import qualified Graphics.Vty                           as V
import qualified Network.WebSockets                     as WS
import qualified Reflex
-- import qualified Control.Concurrent.STM.TMVar           as TM
-- import qualified Control.Concurrent.STM.TBQueue         as TQ

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Core             as Net
import qualified Network.TypedProtocol.Driver           as Net

import           Reflex.Vty.Widget.Extra
import qualified Reflex.Vty.Widget.Input.RichText       as Rich

import Basis hiding (Dynamic)
import Data.Text.Extra
import qualified Data.Text.Zipper.Extra as TZ

import qualified Ground.Hask                      as Hask
import qualified Wire.Peer                        as Wire
import qualified Wire.Protocol                    as Wire

import qualified Shelly

import Lift
import Pipe
import Pipe.Space

import Debug.Reflex
import Debug.TraceErr
import Text.Printf


main :: IO ()
main = do
  timestamp <- Shelly.shelly $ Shelly.run "date" []
  mreq <- execParser $ (info $ optional Wire.parseRequest <**> helper) (traceErr ("MainCLI::main: " <> unpack timestamp) fullDesc)
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req    = fromMaybe (Wire.Run "meta.space") mreq
          -- chan   = channelFromWebsocket conn
      Wire.runClient tracer (client tracer [Wire.Run "meta.dump", req]) $
        channelFromWebsocket conn

client :: forall rej m a
          . (rej ~ Text, m ~ IO, a ~ Wire.Reply)
       => Tracer m String
       -> [Wire.Request]
       -> m (Wire.ClientState rej m a)
client tracer (r0:rs) = pure $
  Wire.ClientRequesting r0 (handleReply rs)
 where
   handleReply _ (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure Wire.ClientDone
   handleReply (r:rs') _ =
     pure $ Wire.ClientRequesting r (handleReply rs')
   handleReply []  (Right (Wire.ReplyValue rep)) = do
     print rep
     case
       withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) rep
       (\(VPoint space) -> do
           traceWith tracer "Got pipes.."
           mainWidget $ spaceInteraction space
       ) of
       Right x -> x
       Left  e -> fail (unpack e)
     pure Wire.ClientDone

spaceInteraction ::
     forall    t m
  .  ReflexVty t m
  => PipeSpace (SomePipe ())
  -> VtyWidget t m (Event t ())
spaceInteraction space = mdo
  screenLayout@ScreenLayout{..} <- computeScreenLayout

  assemblyD :: Reflex.Dynamic t (Maybe (SomePipe ())) <-
    holdDyn Nothing selrChoice

  let constraintD :: Reflex.Dynamic t (Maybe SomeTypeRep)
      constraintD =
        assemblyD
          <&> fmap (somePipeOutSomeTagType >>> snd)
      pipesFromD :: Reflex.Dynamic t [SomePipe ()]
      pipesFromD =
        constraintD
          <&> (pipesFromCstr space
               >>> sortBy (compare `on` somePipeName))

  pipesD <- holdDyn (pipesFromCstr space Nothing) never

  selr@Selector{..} :: Selector t (SomePipe ()) Text <-
    selector
      slSelector
      selToSelrFrameParams
      pipesD -- pipesFromD

  showPane yellow slAssembly $ current assemblyD

  debugVisuals screenLayout selr constraintD pipesFromD

  input <&> fmapMaybe
    (\case
        V.EvKey (V.KChar 'c') [V.MCtrl]
          -> Just () -- terminate
        _ -> Nothing)

 where
   debugVisuals ::
     ScreenLayout t ->
     Selector t (SomePipe ()) Text ->
     Reflex.Dynamic t (Maybe SomeTypeRep) ->
     Reflex.Dynamic t [SomePipe ()] ->
     VtyWidget t m ()
   debugVisuals ScreenLayout{..} Selector{..} cstrD pipesD = do
     err  <- hold "" $ selExt <$> selrSelection
     let showCstrEnv cstr pipes =
           mconcat [ "pipes matching ", pack $ show cstr
                   , ": ", pack $ show (length pipes)]
     dbg1 <- pure . current $ zipDynWith showCstrEnv cstrD pipesD
     dbg2 <- hold (emptySelection "")
       selrSelection
       -- (traceErrEventWith (const "selrSelection fired") selrSelection)
     showPane red   slErrors err
     showPane blue  slDebug1 dbg1
     showPane green slDebug2 $ dbg2 <&>
       \s@Selection{..}-> (,) s $
       let strIx = unColumn selColumn - 1
       in if strIx < T.length selInput && strIx >= 0
          then T.index selInput strIx
          else '*'

   showPane :: (CWidget t m, Show a)
            => V.Attr -> DynRegion t -> Behavior t a -> VtyWidget t m ()
   showPane attr region bx =
     pane region (pure False) $
     richTextStatic attr (T.pack . show <$> bx)

   red, blue, green, yellow :: V.Attr
   (,,,) red blue green yellow =
    (,,,) (foregro V.red) (foregro V.blue) (foregro V.green) (foregro V.yellow)

selToSelrFrameParams ::
     (PostBuild t m, MonadNodeId m, MonadHold t m, MonadFix m)
  => Width
  -> [SomePipe ()]
  -> Selection (SomePipe ()) Text
  -> SelectorFrameParams t m (SomePipe ()) Text
selToSelrFrameParams screenW
 allPipes selection@Selection{selColumn=Column coln, ..} =
  -- selection
  -- -> (string -> parse with locations) & column
  -- -> current token
  -- -> dumb infix subsetting
  case parseLocated selInput of
    Left e  -> SelectorFrameParams [] (selection { selExt = e })
                  (showName . somePipeName)
                  (somePipeSelectable (Width 3, Width 3))
    Right expr@(indexLocated -> index) ->
      let name = case lookupLocatedQName coln index of
                   Nothing -> ""
                   Just qn -> showQName qn
          selectedPipes = infixNameFilter name `Prelude.filter` allPipes
          reservedW = Width 4
      in SelectorFrameParams
           selectedPipes
           (selection
            { selExt =
              T.pack $ printf "col=%s n=%s exp=%s" (show coln) name (show expr) })
           (showName . somePipeName)
           (somePipeSelectable (presentCtx screenW reservedW selectedPipes))
 where
   infixNameFilter hay = T.isInfixOf hay . showName . somePipeName

   somePipeSelectable
     :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t)
     => (Width, Width)
     -> SomePipe ()
     -> Behavior t Bool
     -> VtyWidget t m (SomePipe ())
   somePipeSelectable (Width nameW, Width sigW) sp focusB =
    row $ withSomePipe sp $ \(Pipe pd _) -> do
     fixed (pure nameW) $
       richText (textFocusStyle (foregro V.green) focusB)
         (pure $ T.justifyRight nameW ' ' . showName $ pdName pd)
     fixed 4 $
       richText (textFocusStyle V.defAttr focusB)
         (pure " :: ")
     fixed (pure sigW) $
       richText (textFocusStyle (foregro V.blue) focusB)
         (pd & (pdSig
                >>> showSig
                >>> T.justifyLeft  sigW ' '
                >>> pure))
     pure sp
    where
      textFocusStyle attr focB =
        RichTextConfig $
          selecting (flip V.withBackColor $ V.rgbColor @Integer 1 1 1)
                    (pure attr)
                    focB

   presentCtx :: Width -> Width -> [SomePipe p] -> (Width, Width)
   presentCtx (Width total) (Width reserved) =
     (showName . somePipeName <$>)
     >>> (Prelude.maximum . (T.length <$>))
     >>> (\nameWi -> ( Width nameWi
                     , Width $ total - reserved - nameWi))
   -- showSignature :: Sig -> Text
   -- showSignature (Sig as o) = T.intercalate " ⇨ " $ showType <$> (as <> [o])
   --  where showType SomeType{tName=(showName -> n), tCon} =
   --          case R.tyConName tCon of
   --            "'Point" -> n
   --            "'List"  -> "["<>n<>"]"
   --            "'Set"   -> "{"<>n<>"}"
   --            "'Tree"  -> "♆⇊ "<>n
   --            "'Dag"   -> "♆⇄ "<>n
   --            "'Graph" -> "☸ "<>n
   --            _        -> "??? "<>n
   -- TODO:  ugly as sin.

-- * Boring
--
data ScreenLayout t =
  ScreenLayout
  { slScreenWi      :: !(Reflex.Dynamic t Width)
  , slScreenHe      :: !(Reflex.Dynamic t Height)
  , slSelector      :: !(DynRegion t)
  , slAssembly      :: !(DynRegion t)
  , slErrors        :: !(DynRegion t)
  , slDebug1        :: !(DynRegion t)
  , slDebug2        :: !(DynRegion t)
  }

computeScreenLayout ::
  HasDisplaySize t m
  => m (ScreenLayout t)
computeScreenLayout = do
  wi <- displayWidth
  he <- displayHeight

  pure $
    ScreenLayout
    { slScreenWi    = Width <$> wi
    , slScreenHe    = Height <$> he
    , slSelector    = DynRegion 0  0       wi (he - 8)
    , slAssembly    = DynRegion 0 (he - 7) wi  1
    , slErrors      = DynRegion 0 (he - 5) wi  3
    , slDebug1      = DynRegion 0 (he - 2) wi  1
    , slDebug2      = DynRegion 0 (he - 1) wi  1
    }

-- * UI
--
 -- where
   -- escapable w = do
   --   void w
   --   i <- input
   --   return $ fforMaybe i $ \case
   --     V.EvKey V.KEsc [] -> Just Nothing
   --     _ -> Nothing

   -- look :: PipeSpace (SomePipe ()) -> QName Pipe -> Either Text (SomePipe ())
   -- look space name = lookupSpace name space &
   --   maybeToEither ("No such pipe: " <> showQName name)
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
