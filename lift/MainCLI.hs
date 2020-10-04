--{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main, fetchPSpace) where

import           Safe

import           Codec.Serialise                          (Serialise)
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan.Unagi
                 (Element, InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi          as Unagi
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Control.Tracer
                 (Tracer(..), stdoutTracer, showTracing, traceWith)
import qualified Control.Zipper                         as Zip
import           Data.Dependent.Map (DMap, DSum)
import qualified Data.Dependent.Map                     as DMap
import           Data.Functor.Identity
import           Data.Functor.Misc
import           Data.GADT.Compare
import qualified Data.IntervalMap.FingerTree            as IMap
import qualified Data.List                              as List
import           Data.Map (Map)
import qualified Data.Set.Monad                         as Set
import qualified Data.Set                               as SSet
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

import           Ground.Table                        (VTag(..))
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
      Wire.ReplyValue sv <- Wire.runClient stderr (channelFromWebsocket conn)
                              (pure fetcherClient)
      case withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) sv
           (\(VPoint space) -> space) of
        Right x -> pure x
        Left  e -> fail (unpack e)
 where
   fetcherClient :: Wire.ClientState Text IO Wire.Reply
   fetcherClient = Wire.ClientRequesting fetchPSpaceReq handleReply

   fetchPSpaceReq = Wire.Run "space"

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
  (exeSendW, exeSendR) :: (InChan Execution, OutChan Execution) <- Unagi.newChan
  mainWidget $ reflexVtyApp (exeSendW, exeSendR)

reflexVtyApp :: forall t m.
  (MonadIO (Performable m), ReflexVty t m, PerformEvent t m, PostBuild t m, TriggerEvent t m)
  => (InChan Execution, OutChan Execution)
  -> VtyWidget t m (Event t ())
reflexVtyApp (exeSendW, exeSendR)
  =   getPostBuild
  >>= spawnHostChatterThread hostAddr stderr
  >>= spaceInteraction postExec
 where
   hostAddr = WSAddr "127.0.0.1" (cfWSPortOut defaultConfig) "/"

   postExec :: Execution -> IO ()
   postExec = Unagi.writeChan exeSendW

   spawnHostChatterThread :: WSAddr -> Tracer IO Text -> Event t () -> VtyWidget t m (Event t SomeValue)
   spawnHostChatterThread WSAddr{..} logfd postBuild = performEventAsync $
     ffor postBuild $ \_ fire -> liftIO $ do
       postExec (Execution "space" TPoint (Proxy @(PipeSpace (SomePipe ()))))
       void $ forkIO $ WS.runClient wsaHost wsaPort wsaPath $
         \(channelFromWebsocket -> webSockChan) ->
           void $ Wire.runClient logfd webSockChan $
             client stderr fire exeSendR

data WSAddr
  = WSAddr
  { wsaHost :: String
  , wsaPort :: Int
  , wsaPath :: String
  }

instance GEq CTag
instance GCompare CTag

data Execution where
  Execution :: (ReifyCTag c, Typeable c, Typeable a, Ord a, Ground a) =>
    { eText        :: Text
    , eResultCTag  :: CTag c
    , eResultRep   :: Proxy a
    } -> Execution

client :: forall rej m a
          . (rej ~ Text, m ~ IO, a ~ Wire.Reply)
       => Tracer m Text
       -> (SomeValue -> IO ())
       -> OutChan Execution
       -> m (Wire.ClientState rej m a)
client tr fire reqsChan =
  go
 where
   go :: m (Wire.ClientState rej m a)
   go = do
     exe <- Unagi.readChan reqsChan
     pure . Wire.ClientRequesting (Wire.Run $ eText exe)
          . handleReply $ handler exe

   handler :: Execution -> Wire.Reply -> IO ()
   handler Execution{..} (Wire.ReplyValue rep) =
     case withSomeValue eResultCTag eResultRep rep stripValue of
       Right{} -> fire rep
       Left  e -> fail . unpack $
         "Server response doesn't match Execution: " <> e

   handleReply
     :: (Wire.Reply -> IO ())
     -> Either Text Wire.Reply
     -> IO (Wire.ClientState Text IO Wire.Reply)
   handleReply _      x@(Left  rej) = traceWith tr' x >>
     (pure . Wire.ClientDone . error $ "error: " <> unpack rej)
   handleReply handle x@(Right rep) =
     traceWith tr' x >>
     handle rep >>
     go

   tr' :: Tracer m (Either rej a)
   tr' = Tracer $ traceWith tr . \case
     Left  x -> "error: " <> x
     Right x -> "reply: " <> pack (show x)

data Acceptable
  = APipe  { unAPipe  :: !(SomePipe ()) }
  | AScope { unAScope :: !(Namespace.PointScope (SomePipe ())) }
  deriving (Eq)

instance Show Acceptable where
  show = \case
    APipe  x -> show x
    AScope x -> "Scope " <> T.unpack (showName (Namespace.scopeName x))

acceptablePresent :: Acceptable -> Text
acceptablePresent = \case
  APipe  x -> showQName . somePipeQName $ x
  AScope x -> showName . Namespace.scopeName $ x

acceptableName :: Acceptable -> Name Pipe
acceptableName = \case
  APipe  x -> somePipeName x
  AScope x -> coerceName . Namespace.scopeName $ x

type Acceptance
  = Either Text (Expr (QName Pipe), Expr (Located (CPipe ())))

acceptablePipe :: Acceptable -> Maybe (SomePipe ())
acceptablePipe = \case
  APipe x -> Just x
  _       -> Nothing

spaceInteraction ::
     forall    t m
  .  (ReflexVty t m, PerformEvent t m, MonadIO (Performable m))
  => (Execution -> IO ())
  -> Event t SomeValue
  -> VtyWidget t m (Event t ())
spaceInteraction postExe someValueRepliesE = mdo
  let splitSVByKinds ::
           Event t SomeValue
        -> EventSelectorG t CTag SomeValueKinded
      splitSVByKinds =
        fanG . fmap (\(SomeValue tag sv) ->
                       DMap.singleton tag sv)

      splitSVKByTypes ::
           forall (c :: Con). ReifyCTag c
        => Event t (SomeValueKinded c)
        -> EventSelectorG t CTag (Value c)
      splitSVKByTypes =
        fanG . fmap (\(SomeValueKinded v) ->
                       DMap.singleton (reifyVTag $ Proxy @c) v)

      repliesES :: EventSelectorG t CTag SomeValueKinded
      repliesES = splitSVByKinds someValueRepliesE

      pointRepliesE :: Event t (SomeValueKinded Point)
      pointRepliesE = select repliesES TPoint

      pointRepliesES :: EventSelectorG t VTag (Value Point)
      pointRepliesES = splitSVKByTypes repliesPointE

      spacePointRepliesE :: Event t (Value Point (PipeSpace (SomePipe ())))
      spacePointRepliesE = select pointRepliesES GPipeSpace

  spaceD <- holdDyn emptySpace (stripValue <$> pointRepliesES)

  screenLayout@ScreenLayout{..} <- computeScreenLayout

  assemblyD :: Reflex.Dynamic t (Maybe (SomePipe ())) <-
    -- XXX:  what do we want to see here>?
    --       not the individual components, like now..
    holdDyn Nothing (Just <$> fmapMaybe acceptablePipe selrChoice)
    >>= holdUniqDyn

  let constraintD :: Reflex.Dynamic t (Maybe SomeTypeRep)
      constraintD = trdyn (\c-> mconcat ["choice: ", show c])
                    assemblyD
                    <&> fmap (somePipeOutSomeCTagType >>> snd)
      pipesFromD :: Reflex.Dynamic t [SomePipe ()]
      pipesFromD  = zipDynWith pipesFromCstr spaceD constraintD
                    <&> sortBy (compare `on` somePipeName)

  sfp0 :: SelectorFrameParams t m Acceptable Acceptance <-
          selToSelrFrameParams
            <$> sample (current spaceD)
            <*> (Width <$> sample (current $ _dynRegion_width slaySelector))
            <*> pure (emptySelection $ Left mempty)
  sfpD :: Reflex.Dynamic t (SelectorFrameParams t m Acceptable Acceptance) <- holdDyn sfp0 $
            attachPromptlyDynWith rcons2
              (zipDyn
                spaceD
                (Width <$> _dynRegion_width slaySelector))
              selrSelection
            <&> uncurry3 selToSelrFrameParams

  Selector{..} :: Selector t m Acceptable Acceptance <-
    (selector slaySelector sfpD
     :: VtyWidget t m (Selector t m Acceptable Acceptance))

  let astExpE = selExt . sfpSelection <$> updated sfpD

  visD <- holdDyn "" (either id (pack . show) . fmap snd <$> astExpE)
  textPane yellow slayAssembly $ current visD

  debugVisuals screenLayout astExpE constraintD pipesFromD

  input <&>
    fmapMaybe
      (\case
          V.EvKey (V.KChar 'c') [V.MCtrl]
            -> Just () -- terminate
          _ -> Nothing)

 where
   debugVisuals ::
     ScreenLayout t ->
     Event t Acceptance ->
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
     textPane red   slayErrors redD
     showPane blue  slayDebug1 blueD
     showPane green slayDebug2 greenD

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

selToSelrFrameParams :: forall t m
   . (PostBuild t m, MonadNodeId m, MonadHold t m, MonadFix m)
  => SomePipeSpace ()
  -> Width
  -> Selection
       Acceptable
       (Either Text ( Expr (QName Pipe)
                    , Expr (Located (CPipe ()))))
  -> SelectorFrameParams t m Acceptable Acceptance
selToSelrFrameParams spc screenW selection@Selection{selColumn=Column coln, ..} = either errFrameParams id $ do

  let focusChar = if T.length selInput >= coln && coln > 0 then T.index selInput (coln - 1) else 'Ø'
      look = lookupPipe spc

  ast :: Expr (Located (QName Pipe)) <- parse selInput
  cpipe <- analyse look ast

  let -- determine the completable
      indexAst = indexLocated ast
      indexCPipe = indexLocated cpipe
      inputName = fromMaybe mempty $ lookupLocated coln indexAst
      inputCPipe = lookupLocated coln indexCPipe
      -- componentise into scope name and tail
      (scopeName, Name rawName) = unconsQName inputName &
                                  fromMaybe (mempty, Name "")
      -- determine the current scope
      mScope = Namespace.lookupSpaceScope (coerceQName scopeName) (psSpace spc)
      -- determine immediate children of named scope, matching the stem restriction
      subScNames = childScopeQNamesAt (coerceQName scopeName) spc
                   & Prelude.filter (T.isInfixOf rawName . showName . lastQName)
      -- determine pipes within named scope
      scPipes = Namespace.selectFromScope
                  (\k _ -> rawName `T.isInfixOf` showName k)
                  <$> mScope
                & fromMaybe mempty
                & restrictByCPipe inputCPipe
      subScopes = catMaybes $ flip scopeAt spc <$> subScNames
      --
      sels = (APipe <$> scPipes) <> (AScope <$> subScopes)
      --
      compR = compile opsDesc look ast

  pure $ computeFrameParams (locVal <$> ast) cpipe sels

 where
   -- | Given a set of available pipes,
   --   restrict that,
   --   by matching them against the pipe signature provided by type checker
   --   for the current input context (position inside partially resolved AST).
   restrictByCPipe :: Maybe (CPipe ()) -> [SomePipe ()] -> [SomePipe ()]
   restrictByCPipe Nothing  xs = xs
   restrictByCPipe (Just p) xs = Prelude.filter (matchMSig $ cpArgs p) xs
    where
      matchMSig :: MSig -> SomePipe () -> Bool
      matchMSig sig sp =
        traceErr ("verdict on " <> spname <> "::" <> show spsig <> " vs " <> show msig <> ": " <> show r) r
       where
         msig = sOut sig   : sArgs sig
         psig = sOut spsig : sArgs spsig
         r =
           (msig == [Nothing] || length msig == length psig)
           && go (zip msig psig)
         spsig = somePipeSig sp
         go :: [(Maybe SomeType, I SomeType)] -> Bool
         go = foldl (\acc x -> acc && uncurry tyMatch x) True
         spname = unpack $ showName $ somePipeName sp
         tyMatch :: Maybe SomeType -> I SomeType -> Bool
         tyMatch Nothing   _    = traceErr ("match " <> spname <> ": *")
                                  True
         tyMatch (Just x) (I y) =
           traceErr ("match " <> spname <> ":" <> show x <> "|" <> show y <>
                     ": " <> show (x == y))
           $ x == y

   computeFrameParams
     :: Expr (QName Pipe)
     -> Expr (Located (CPipe ()))
     -> [Acceptable]
     -> SelectorFrameParams t m Acceptable Acceptance
   computeFrameParams ast expr@(indexLocated -> index) sels =
     SelectorFrameParams
       sels
       charCompletionP
       (selection { selExt =
                    -- traceErr (mconcat
                    --           [ show coln, " "
                    --           , show focusChar, " "
                    --           , unpack selInput, " -> "
                    --           , show exp
                    --           ]) $
                    Right (ast, expr) })
       acceptablePresent
       (somePipeSelectable (presentCtx screenW reservedW sels))

   errFrameParams err =
     SelectorFrameParams []
       charCompletionP
       (selection { selExt = Left err })
       (showName . acceptableName)
       (somePipeSelectable (reservedW, reservedW))
   reservedW = Width 4
   charCompletionP leftC newC =
     newC == ' ' || newC == '\t' || (newC == '.' && isJust leftC && leftC /= Just ' ')
   infixNameFilter needle = T.isInfixOf needle . showName . somePipeName

   somePipeSelectable
     :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t)
     => (Width, Width)
     -> Acceptable
     -> Behavior t Bool
     -> VtyWidget t m Acceptable
   somePipeSelectable (Width nameW, Width sigW) x@(APipe sp) focusB =
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
     pure x
   somePipeSelectable (Width nameW, Width sigW) x@(AScope scope) focusB =
    row $ do
     fixed (pure nameW) $
       richText (textFocusStyle (foregro V.green) focusB)
         (pure $ T.justifyRight nameW ' ' . showName $ Namespace.scopeName scope)
     fixed 4 $
       richText (textFocusStyle V.defAttr focusB)
         (pure " :: ")
     fixed (pure sigW) $
       richText (textFocusStyle (foregro V.blue) focusB)
         (pure . pack . show $ Namespace.scopeSize scope)
     pure x

   textFocusStyle attr focB =
     RichTextConfig $
       selecting (flip V.withBackColor $ V.rgbColor @Integer 1 1 1)
                 (pure attr)
                 focB

   presentCtx ::
     Width
     -> Width
     -> [Acceptable]
     -> (Width, Width)
   presentCtx (Width total) (Width reserved) =
     fmap acceptablePresent
     >>> (Prelude.maximum . (T.length <$>))
     >>> (\nameWi -> ( Width nameWi
                     , Width $ total - reserved - nameWi))

-- * Boring
--
data ScreenLayout t =
  ScreenLayout
  { slayScreenWi      :: !(Reflex.Dynamic t Width)
  , slayScreenHe      :: !(Reflex.Dynamic t Height)
  , slaySelector      :: !(DynRegion t)
  , slayAssembly      :: !(DynRegion t)
  , slayErrors        :: !(DynRegion t)
  , slayDebug1        :: !(DynRegion t)
  , slayDebug2        :: !(DynRegion t)
  }

computeScreenLayout ::
  HasDisplaySize t m
  => m (ScreenLayout t)
computeScreenLayout = do
  wi <- displayWidth
  he <- displayHeight

  pure $
    ScreenLayout
    { slayScreenWi    = Width <$> wi
    , slayScreenHe    = Height <$> he
    , slaySelector    = DynRegion 0  0       wi (he - 8)
    , slayAssembly    = DynRegion 0 (he - 7) wi  1
    , slayErrors      = DynRegion 0 (he - 5) wi  3
    , slayDebug1      = DynRegion 0 (he - 2) wi  1
    , slayDebug2      = DynRegion 0 (he - 1) wi  1
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
   -- case (,) (lookupSpace "fromrep" space)
   --            (lookupSpace "unit" space)
   --     of (,) (Just fromrep)
   --            (Just unit) -> do
   --          case apply (app opsDesc) fromrep
   --               (SomeValue
   --                (SomeKindValue
   --                 TPoint (VPoint (Name "Unit" :: Name Type)))) of
   --            Left e -> putStrLn (unpack e)
   --            Right p -> putStrLn (show p)
