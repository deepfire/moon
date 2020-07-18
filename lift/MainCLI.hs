--{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main, fetchPSpace) where

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
import qualified Data.Sequence                          as Seq
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
import Lift.Pipe
import Pipe
import Pipe.Space
import qualified Namespace

import Debug.Reflex
import Debug.TraceErr
import Text.Printf


fetchPSpace :: IO (PipeSpace (SomePipe ()))
fetchPSpace =
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      Wire.ReplyValue sv <- Wire.runClient
                              stdoutTracer
                              (pure fetcherClient)
                              (channelFromWebsocket conn)
      case withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) sv
           (\(VPoint space) -> space) of
        Right x -> pure x
        Left  e -> fail (unpack e)
 where
   fetcherClient :: Wire.ClientState Text IO Wire.Reply
   fetcherClient = Wire.ClientRequesting fetchPSpaceReq handleReply

   fetchPSpaceReq = Wire.Run "meta.space"

   handleReply (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure (Wire.ClientDone undefined)
   handleReply (Right r@(Wire.ReplyValue rep)) = do
     print rep
     case withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) rep
          (\(VPoint space) -> space) of
       Right _ -> pure $ Wire.ClientDone r
       Left  e -> fail (unpack e)

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
      pure ()

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
     pure (Wire.ClientDone $ error $ "error: " <> unpack rej)
   handleReply (r:rs') _ =
     pure $ Wire.ClientRequesting r (handleReply rs')
   handleReply []  (Right r@(Wire.ReplyValue rep)) = do
     print rep
     case
       withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) rep
       (\(VPoint space) -> do
           traceWith tracer "Got pipes.."
           mainWidget $ spaceInteraction space
       ) of
       Right x -> pure $ Wire.ClientDone r
       Left  e -> fail (unpack e)

spaceInteraction ::
     forall    t m
  .  ReflexVty t m
  => PipeSpace (SomePipe ())
  -> VtyWidget t m (Event t ())
spaceInteraction space = mdo
  screenLayout@ScreenLayout{..} <- computeScreenLayout

  assemblyD :: Reflex.Dynamic t (Maybe (SomePipe ())) <-
    -- XXX:  what do we want to see here>?
    --       not the individual components, like now..
    holdDyn Nothing (Just <$> selrChoice)
    >>= holdUniqDyn

  let constraintD :: Reflex.Dynamic t (Maybe SomeTypeRep)
      constraintD = trdyn (\c-> mconcat ["choice: ", show c])
                    assemblyD
                    <&> fmap (somePipeOutSomeTagType >>> snd)
      pipesFromD :: Reflex.Dynamic t [SomePipe ()]
      pipesFromD  = constraintD
                    <&> (pipesFromCstr space
                         >>> sortBy (compare `on` somePipeName))

  pipesD <- holdDyn (pipesFromCstr space Nothing) never

  sfp0 <- selToSelrFrameParams space
            <$> (Width <$> sample (current $ _dynRegion_width slSelector))
            <*> sample (current pipesD)
            <*> pure (emptySelection $ Left mempty)
  sfpD <- holdDyn sfp0 $
            attachPromptlyDynWith lcons2
              (Width <$> _dynRegion_width slSelector)
              (attachPromptlyDynWith (,)
               pipesD
               selrSelection)
            <&> uncurry3 (selToSelrFrameParams space)

  Selector{..} ::
    Selector t m (SomePipe ())
      (Either Text (Expr (QName Pipe), Expr (Located (CPipe ())))) <-
    selector slSelector sfpD

  let astExpE = selExt . sfpSelection <$> updated sfpD

  visD <- holdDyn "" (either id (pack . show) . fmap snd <$> astExpE)
  textPane yellow slAssembly $ current visD

  debugVisuals screenLayout astExpE constraintD pipesFromD

  input <&> fmapMaybe
    (\case
        V.EvKey (V.KChar 'c') [V.MCtrl]
          -> Just () -- terminate
        _ -> Nothing)

 where
   debugVisuals ::
     ScreenLayout t ->
     Event t (Either Text (Expr (QName Pipe), Expr (Located (CPipe ())))) ->
     Reflex.Dynamic t (Maybe SomeTypeRep) ->
     Reflex.Dynamic t [SomePipe ()] ->
     VtyWidget t m ()
   debugVisuals ScreenLayout{..} selExt cstrD pipesD = do
     redD <- hold "" $ either id (pack . show) . fmap fst <$> selExt
     let showCstrEnv cstr pipes =
           mconcat [ "pipes matching ", pack $ show cstr
                   , ": ", pack $ show (length pipes)]
     blueD  <- pure . current $ zipDynWith showCstrEnv cstrD pipesD
     greenD <- hold "" (either id (pack . show) <$> selExt)
     textPane red   slErrors redD
     showPane blue  slDebug1 blueD
     showPane green slDebug2 greenD

   showPane :: (CWidget t m, Show a)
            => V.Attr -> DynRegion t -> Behavior t a -> VtyWidget t m ()
   showPane attr region bx =
     pane region (pure False) $
     richTextStatic attr (T.pack . show <$> bx)

   textPane :: (CWidget t m)
            => V.Attr -> DynRegion t -> Behavior t Text -> VtyWidget t m ()
   textPane attr region bx =
     pane region (pure False) $
     richTextStatic attr bx

   red, blue, green, yellow :: V.Attr
   (,,,) red blue green yellow =
    (,,,) (foregro V.red) (foregro V.blue) (foregro V.green) (foregro V.yellow)

selToSelrFrameParams ::
     (PostBuild t m, MonadNodeId m, MonadHold t m, MonadFix m)
  => SomePipeSpace ()
  -> Width
  -> [SomePipe ()]
  -> Selection
       (SomePipe ())
       (Either Text ( Expr (QName Pipe)
                    , Expr (Located (CPipe ()))))
  -> SelectorFrameParams
       t m
       (SomePipe ())
       (Either Text ( Expr (QName Pipe)
                    , Expr (Located (CPipe ()))))
selToSelrFrameParams spc screenW allPipes selection@Selection{selColumn=Column coln, ..} =
 let focusChar = if T.length selInput >= coln && coln > 0 then T.index selInput (coln - 1) else 'Ø'
     look = lookupPipe spc
 in
  case do
    ast   <- parse selInput
    cpipe <- analyse look ast
    pure ( locVal <$> ast
         , cpipe
         , compile opsDesc look ast
         )
  of
    Left err -> SelectorFrameParams []
                  charCompletionP
                  (selection { selExt = Left err })
                  (showName . somePipeName)
                  (somePipeSelectable (Width 3, Width 3))
    Right (ast, expr@(indexLocated -> index), _compRes) ->
      let reservedW = Width 4
          name = case lookupLocated coln index of
                   Nothing -> ""
                   Just (CSomePipe _args p) -> showName (somePipeName p)
                   Just (CFreePipe _args n) -> showQName n
          subScopeNames = childScopeNamesAt ((QName Seq.empty) :: QName Scope) spc
          pipesSubset = infixNameFilter name `Prelude.filter` allPipes
      in SelectorFrameParams
           pipesSubset
           charCompletionP
           (selection { selExt =
                        traceErr (mconcat
                                  [ show coln, " "
                                  , show focusChar, " "
                                  , unpack selInput, " -> "
                                  , show expr]) $
                        Right (ast, expr) })
           (showQName . somePipeQName)
           (somePipeSelectable (presentCtx screenW reservedW pipesSubset))
 where
   charCompletionP leftC newC =
     newC == ' ' || newC == '\t' || (newC == '.' && (isJust leftC && leftC /= Just ' '))
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
