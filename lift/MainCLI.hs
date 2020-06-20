--{-# OPTIONS_GHC -Wall #-}

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

import Lift
import Pipe
import Pipe.Space

import Debug.Reflex
import Debug.TraceErr
import Text.Printf


main :: IO ()
main = do
  mreq <- execParser $ (info $ optional Wire.parseRequest <**> helper) (traceErr "foo" fullDesc)
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req    = fromMaybe (Wire.Run "meta.space") mreq
          -- chan   = channelFromWebsocket conn
      Wire.runClient tracer (client tracer req) $ channelFromWebsocket conn

client :: forall rej m a
          . (rej ~ Text, m ~ IO, a ~ Wire.Reply)
       => Tracer m String
       -> Wire.Request
       -> m (Wire.ClientState rej m a)
client tracer req = pure $
  Wire.ClientRequesting req handleReply
 where
   handleReply (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure Wire.ClientDone
   handleReply (Right (Wire.ReplyValue rep)) = do
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

  repFromD :: Reflex.Dynamic t (Maybe SomeTypeRep) <-
    pure . pure $ Nothing

  pipesFromD  <- pure $ pipesFromCstr space <$> repFromD
  pipesSorteD <- pure $ pipesFromD <&>
                        sortBy (compare `on` somePipeName)

  menuInputStateD <-
    selectionUI
      slSelectorReg
      computeMenuInputState
      pipesSorteD

  debugVisuals screenLayout menuInputStateD

  input <&> fmapMaybe
    (\case
        V.EvKey (V.KChar 'c') [V.MCtrl]
          -> Just ()
        _ -> Nothing)

 where
   debugVisuals ScreenLayout{..} menuStateD = do
     showPane red   slErrorsReg $ misExt <$> current menuStateD
     showPane blue  slDebug1Reg $ current (length . misElems <$> menuStateD)
     showPane green slDebug2Reg $ current (misSelection <$> menuStateD)

   showPane :: (CWidget t m, Show a)
            => V.Attr -> DynRegion t -> Behavior t a -> VtyWidget t m ()
   showPane attr region bx =
     pane region (pure False) $
     richTextStatic attr (T.pack . show <$> bx)

   red, blue, green :: V.Attr
   (,,) red blue green =
    (,,) (foregro V.red) (foregro V.blue) (foregro V.green)

computeMenuInputState ::
     (PostBuild t m, MonadNodeId m, MonadHold t m, MonadFix m)
  => Width
  -> [SomePipe ()]
  -> Selection (SomePipe ())
  -> MenuInputState t m (SomePipe ()) Text
computeMenuInputState screenW allPipes selection@Selection{..} =
  case parseLocated sInput of
    Left e  -> MenuInputState selection [] (somePipeSelectable (Width 3, Width 3))
                              (showName . somePipeName) e
    Right expr@(indexLocated -> index) ->
      let name = case lookupLocatedQName sColumn index of
                   Nothing -> ""
                   Just qn -> showQName qn
          selectedPipes = infixNameFilter name `Prelude.filter` allPipes
          reservedW = Width 4
      in MenuInputState
           selection
           selectedPipes
           (somePipeSelectable (presentCtx screenW reservedW selectedPipes))
           (showName . somePipeName)
           (T.pack
            $ printf "col=%s n=%s exp=%s" (show sColumn) name (show expr))
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
  , slSelectorReg   :: !(DynRegion t)
  , slErrorsReg     :: !(DynRegion t)
  , slDebug1Reg     :: !(DynRegion t)
  , slDebug2Reg     :: !(DynRegion t)
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
    , slSelectorReg = DynRegion 0  0       wi (he - 6)
    , slErrorsReg   = DynRegion 0 (he - 5) wi  3
    , slDebug1Reg   = DynRegion 0 (he - 2) wi  1
    , slDebug2Reg   = DynRegion 0 (he - 1) wi  1
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
