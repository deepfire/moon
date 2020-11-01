--{-# OPTIONS_GHC -dshow-passes -dppr-debug -ddump-rn -ddump-tc #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

import qualified Control.Concurrent.Async               as Async
import           Control.Concurrent.Chan.Unagi            (InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi          as Unagi
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Control.Tracer                           (Tracer(..), traceWith)
import           Data.Text                                (Text)

import           Reflex                            hiding (Request)
import           Reflex.Network
import           Reflex.Vty                        hiding (Request)
import qualified Data.Text                              as T
import qualified Graphics.Vty                           as V
import qualified Network.WebSockets                     as WS


import           Reflex.Vty.Widget.Extra
import           Reflex.Vty.Widget.Selector


import Basis hiding (Dynamic)
import Debug.Reflex
-- import Debug.TraceErr

import Dom.CTag
import Dom.Error
import Dom.Expr
import Dom.Ground
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Sig
import Dom.SomeType
import Dom.SomeValue
import Dom.Space
import Dom.Space.Pipe
import Dom.Space.SomePipe
import Dom.Value
import Dom.VTag

import Ground.Expr
import Ground.Table

import qualified Wire.Peer                        as Wire
import qualified Wire.Protocol                    as Wire

import Lift hiding (main)
import Lift.Pipe

import Reflex.SomeValue



main :: IO ()
main = do
  sealGround
  mainWidget reflexVtyApp

reflexVtyApp :: forall t m.
  (MonadIO (Performable m), ReflexVty t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadIO m)
  => VtyWidget t m (Event t ())
reflexVtyApp = do
  init <- getPostBuild

  (exeSendW, exeSendR) :: (InChan (Execution t), OutChan (Execution t)) <-
    liftIO Unagi.newChan

  someValuE <- spawnHostChatterThread hostAddr exeSendR exeSendW stderr init

  spaceInteraction (postExec exeSendW) someValuE
 where
   hostAddr = WSAddr "127.0.0.1" (cfWSPortOut defaultConfig) "/"

   postExec :: InChan (Execution t) -> Execution t -> IO ()
   postExec = Unagi.writeChan

   spawnHostChatterThread ::
        WSAddr
     -> OutChan (Execution t)
     -> InChan (Execution t)
     -> Tracer IO Text
     -> Event t ()
     -> VtyWidget t m (Event t SomeValue)
   spawnHostChatterThread WSAddr{..} exeSendR exeSendW logfd postBuild = performEventAsync $
     ffor postBuild $ \_ fire -> liftIO $ do
       postExec exeSendW (Execution TPoint VPipeSpace "space" "space" never)
       thd <- Async.async $ WS.runClient wsaHost wsaPort wsaPath $
         \(channelFromWebsocket -> webSockChan) ->
           void $ Wire.runClient logfd webSockChan $
             client stderr fire exeSendR
       Async.link thd

data WSAddr
  = WSAddr
  { wsaHost :: String
  , wsaPort :: Int
  , wsaPath :: String
  }

data Execution t =
  forall c a.
  (Typeable c, Typeable a, Show a) =>
  Execution
  { eResCTag  :: CTag c
  , eResVTag  :: VTag a
  , eText     :: Text
  , eExpr     :: Expr (Located (QName Pipe))
  , eReply    :: Event t (Value c a)
  }

client :: forall rej m a t
          . (rej ~ Error, m ~ IO, a ~ Wire.Reply)
       => Tracer m Text
       -> (SomeValue -> IO ())
       -> OutChan (Execution t)
       -> m (Wire.ClientState rej m a)
client tr fire reqsChan =
  go
 where
   go :: m (Wire.ClientState rej m a)
   go = do
     exe <- Unagi.readChan reqsChan
     pure . Wire.ClientRequesting (Wire.Run $ eExpr exe)
          . handleReply $ handler exe

   handler :: Execution t -> Wire.Reply -> IO ()
   handler Execution{..} (Wire.ReplyValue rep) =
     case withSomeValue eResCTag eResVTag rep stripValue of
       Right{} -> fire rep
       Left (Error e) -> fail . unpack $
         "Server response doesn't match Execution: " <> e

   handleReply
     :: (Wire.Reply -> IO ())
     -> Fallible Wire.Reply
     -> IO (Wire.ClientState rej IO Wire.Reply)
   handleReply _      x@(Left (Error e)) = traceWith tr' x >>               go
   handleReply handle x@(Right rep)      = traceWith tr' x >> handle rep >> go

   tr' :: Tracer m (Either Error a)
   tr' = Tracer $ traceWith tr . \case
     Left (Error x) -> "error: " <> x
     Right x -> "reply: " <> pack (show x)

data Assembly
  = Runnable
    { aText  :: !Text
    , aExpr  :: !(Expr (Located (QName Pipe)))
    -- XXX: this is a terrible conflation:
    --      one does not describe the other!
    , aPipe  :: !(SomePipe ())
    }
  | Incomplete
    { aError :: !Error
    }

instance Show Assembly where
  show = \case
    Runnable{..} -> "#<ASM " <> show aExpr <> ">"
    Incomplete e -> unpack $ showError e

data Acceptable
  = AAssembly { unAAssembly :: !Assembly }
  | AScope { unAScope :: !(PointScope (SomePipe ())) }

instance Show Acceptable where
  show = \case
    AAssembly r -> show $ aExpr r
    AScope x -> "Scope " <> T.unpack (showName (scopeName x))

acceptablePresent :: Acceptable -> Text
acceptablePresent = \case
  AAssembly r -> showQName . somePipeQName $ aPipe r
  AScope x -> showName . scopeName $ x

acceptableName :: Acceptable -> Name Pipe
acceptableName = \case
  AAssembly r -> somePipeName $ aPipe r
  AScope x -> coerceName . scopeName $ x

type Acceptance
  = Fallible (Expr (QName Pipe), Expr (Located (PartPipe ())))

mkAssembly ::
     (QName Pipe -> Maybe (SomePipe ()))
  -> Acceptable
  -> Fallible Assembly
mkAssembly look = \case
  AAssembly a -> enrich a <$> compile opsDesc look (aExpr a)
  _ -> fall "Not a pipe"
 where enrich a p = a { aPipe = p }

spaceInteraction ::
     forall    t m
  .  (ReflexVty t m, PerformEvent t m, MonadIO (Performable m))
  => (Execution t -> IO ())
  -> Event t SomeValue
  -> VtyWidget t m (Event t ())
spaceInteraction postExe someValueRepliesE = mdo
  let pointRepliesE :: Event t (SomeValueKinded Point)
      pointRepliesE = selectG (splitSVByKinds someValueRepliesE) TPoint
      spacePointRepliesE :: Event t (Value Point (PipeSpace (SomePipe ())))
      spacePointRepliesE = selectG (splitSVKByTypes pointRepliesE) VPipeSpace

  spaceD :: Dynamic t (PipeSpace (SomePipe ())) <-
    holdDyn mempty (stripValue <$> spacePointRepliesE)

  runnableD :: Dynamic t (Fallible Assembly) <-
    holdDyn (Left $ Error "- no pipe -") $
    attach (current spaceD) (selrAccepted selr)
    <&> uncurry (\spc acceptedAsm ->
                   mkAssembly (lookupSomePipe spc) acceptedAsm)

    -- accD <- holdDyn Nothing (Just <$> selrAccepted selr)
    -- (\acc spc -> Just $ runnablePipe (lookupSomePipe spc) acc)
    --   <$> accD
    --   <*> spaceD
    -- >>= holdUniqDyn

  let constraintD :: Dynamic t (Maybe SomeTypeRep)
      constraintD = -- trdyn (\c-> mconcat ["choice: ", show c])
                    runnableD
                    <&> (fmap (aPipe >>> somePipeOutSomeCTagType >>> snd)
                         >>> either (const Nothing) Just)
      pipesFromD :: Dynamic t [SomePipe ()]
      pipesFromD  = zipDynWith pipesFromCstr spaceD constraintD
                    <&> sortBy (compare `on` somePipeName)

  -- Ok, so
  -- One problem is that the selector is apparently only triggered
  -- on what it considers significant event, like completion.  Whew!
  --
  -- Another problem is that runnableD fires even if we don't have
  -- a complete pipe to run.
  let showErr :: Error -> VtyWidget t m (Maybe (Execution t))
      showErr =
        (>> pure Nothing)
        . boxTitle (pure roundedBoxStyle) "Error"
        . text . pure . showError
  executionE :: Event t (Execution t)
    <- fmap catMaybes . performEvent $
       (updated (trdyn (("runnable: "<>) . show) runnableD) <&>) $
       either (pure . const Nothing) $
       \case
         Runnable{..} -> do
           case withSomeGroundPipe aPipe $
                 \(Pipe{pDesc} :: Pipe Ground kas o ()) ->
                   let ctag = descOutCTag pDesc :: CTag (TypesC o)
                       vtag = descOutVTag pDesc :: VTag (TypesV o)
                   in Execution ctag vtag aText aExpr $
                        selectG (splitSVKByTypes $ selectG (splitSVByKinds someValueRepliesE) ctag) vtag of
             Just exe -> do
               traceM "Ground-pipe Runnable"
               liftIO $ postExe exe
               traceM "posted exe"
               pure $ Just exe
             Nothing -> do
               traceM "non-Ground-pipe Runnable"
               pure Nothing
         _ -> pure Nothing

  ((((selrFrameParamsD, selr), _), _), _) <-
    splitV (pure (\x->x-8)) (pure $ join (,) True)
           (splitH (pure $ flip div 2) (pure $ join (,) True)
                   (splitV (pure (\x->x-2)) (pure $ join (,) True)
                      (selectorWidget spaceD (summaryWidget executionE))
                      blank)
                   (presentWidget executionE)) $
           (feedbackWidget
             selrFrameParamsD
             pipesFromD
             constraintD
             runnableD)

  waitForTheEndOf input

 where
   summaryWidget ::
        Event t (Execution t)
     -> VtyWidget t m ()
   summaryWidget exE = do
     networkHold (res $ presentText "-- no valid pipe --") $
       exE <&> \e -> res $
           withExecutionReply
             (pres . fmap (("• " <>) . showT . stripValue))
             (pres . fmap (("list of " <>) . showT . length . stripValue))
             (pres . fmap (("set of " <>) . showT . length . stripValue))
             (const $ presentText "tree")
             (const $ presentText "dag")
             (const $ presentText "graph")
             e
     pure ()
    where
      res x = row $ do
        width <- displayWidth
        fixed (pure 8) $
          richTextStatic blue (pure "Result: ")
        fixed (width - 8) x
      pres = presentTextE "-- no data yet --"

   presentWidget ::
        Event t (Execution t)
     -> VtyWidget t m (Dynamic t ())
   presentWidget exE =
     networkHold
       (boxStatic roundedBoxStyle $ presentText
        "You are standing at the end of a road before a small brick building.") $
       exE <&> \e@Execution{..} ->
         boxTitle (pure roundedBoxStyle) (" "<>eText<>" ") $
           withExecutionReply
             presentPoint
             (presentList . fmap stripValue)
             (presentList . fmap stripValue)
             (const $ presentText "trees not presentable yet")
             (const $ presentText "DAGs not presentable yet")
             (const $ presentText "graphs not presentable yet")
             e

   presentList :: Show a => Event t [a] -> VtyWidget t m ()
   presentList e =
     selectionMenu
       (focusButton (buttonPresentText
                      richTextFocusConfigDef
                      showT)
        >>> fmap fbFocused)
       (Index 0)
       e
       <&> pure ()

   presentPoint :: Show a => Event t (Value Point a) -> VtyWidget t m ()
   presentPoint e =
     text =<< hold "-- no data yet --" (e <&> pack . show . stripValue)

   presentText :: Text -> VtyWidget t m ()
   presentText = text . pure

   presentTextE :: Text -> Event t Text -> VtyWidget t m ()
   presentTextE def = (text =<<) . hold def

   withExecutionReply ::
        (forall a. Show a => Event t (Value Point a) -> b)
     -> (forall a. Show a => Event t (Value List  a) -> b)
     -> (forall a. Show a => Event t (Value 'Set  a) -> b)
     -> (forall a. Show a => Event t (Value Tree  a) -> b)
     -> (forall a. Show a => Event t (Value Dag   a) -> b)
     -> (forall a. Show a => Event t (Value Graph a) -> b)
     -> Execution t
     -> b
   withExecutionReply fp fl fs ft fd fg Execution{..} =
     case eResCTag of
       TPoint -> fp eReply
       TList  -> fl eReply
       TSet   -> fs eReply
       TTree  -> ft eReply
       TDag   -> fd eReply
       TGraph -> fg eReply

   feedbackWidget ::
        Dynamic t (SelectorFrameParams t m Acceptable Acceptance)
     -> Dynamic t [SomePipe ()]
     -> Dynamic t (Maybe SomeTypeRep)
     -> Dynamic t (Fallible Assembly)
     -> VtyWidget t m ()
   feedbackWidget selrFrameParamsD pipesFromD constraintD runnableD = do
     let astExpE = selExt . sfpSelection <$> updated selrFrameParamsD

     visD   <- holdDyn "" $ either showError (pack . show . snd) <$> astExpE
     redD   <- hold ""    $ either showError (pack . show . fst) <$> astExpE
     blueD  <- pure . current $ zipDynWith showCstrEnv constraintD pipesFromD
     greenD <- hold ""    $ either showError (showT . aExpr)
                              <$> updated runnableD

     splitV (pure $ const 1) (pure $ join (,) True)
       (richTextStatic yellow . current $
         visD)
      (splitV (pure $ const 3) (pure $ join (,) True)
       (richTextStatic red $
         redD)
      (splitV (pure $ const 1) (pure $ join (,) True)
       (richTextStatic blue $
         blueD)
      (richTextStatic green $
         greenD)))

     pure ()
    where
      showCstrEnv cstr pipes =
        mconcat [ "pipes matching ", pack $ show cstr
                , ": ", pack $ show (length pipes)
                ]

   selectorWidget ::
        Dynamic t (PipeSpace (SomePipe ()))
     -> VtyWidget t m ()
     -> VtyWidget t m (Dynamic t (SelectorFrameParams t m Acceptable Acceptance),
                       Selector t m Acceptable Acceptance)
   selectorWidget spaceD summaryW = mdo
     width <- displayWidth

     sfp0 :: SelectorFrameParams t m Acceptable Acceptance <-
       selToSelrFrameParams
         <$> sample (current spaceD)
         <*> (Width <$> sample (current width))
         <*> pure (emptySelection $ Left $ Error "")

     sfpD :: Dynamic t (SelectorFrameParams t m Acceptable Acceptance) <- holdDyn sfp0 $
       attachPromptlyDynWith rcons2
         (zipDyn spaceD (Width <$> width))
         (selrSelection selr)
         <&> uncurry3 selToSelrFrameParams

     selr :: Selector t m Acceptable Acceptance <-
       (selector sfpD summaryW
        :: VtyWidget t m (Selector t m Acceptable Acceptance))

     pure (sfpD, selr)

   red, blue, green, yellow :: V.Attr
   (,,,) red blue green yellow =
    (,,,) (foregro V.red) (foregro V.blue) (foregro V.green) (foregro V.yellow)

   waitForTheEndOf inp =
    inp <&>
      fmapMaybe
        (\case
            V.EvKey (V.KChar 'c') [V.MCtrl]
              -> Just () -- terminate
            _ -> Nothing)

selToSelrFrameParams :: forall t m
   . (PostBuild t m, MonadNodeId m, MonadHold t m, MonadFix m)
  => SomePipeSpace ()
  -> Width
  -> Selection
       Acceptable
       (Fallible ( Expr (QName Pipe)
                 , Expr (Located (PartPipe ()))))
  -> SelectorFrameParams t m Acceptable Acceptance
selToSelrFrameParams spc screenW selection@Selection{selColumn=Column coln, ..} = either errFrameParams id $ do

  let focusChar = if T.length selInput >= coln && coln > 0 then T.index selInput (coln - 1) else 'Ø'
      look = lookupSomePipe spc

  ast :: Expr (Located (QName Pipe)) <- parseGroundExpr selInput
  partPipe <- analyse look ast

  let -- determine the completable
      indexAst = indexLocated ast
      indexPartPipe = indexLocated partPipe
      inputName = fromMaybe mempty $ lookupLocated coln indexAst
      inputPartPipe = lookupLocated coln indexPartPipe
      -- componentise into scope name and tail
      (scopeName, Name rawName) = unconsQName inputName &
                                  fromMaybe (mempty, Name "")
      -- determine the current scope
      mScope = lookupSpaceScope (coerceQName scopeName) (psSpace spc)
      -- determine immediate children of named scope, matching the stem restriction
      subScNames = childPipeScopeQNamesAt (coerceQName scopeName) spc
                   & Prelude.filter (T.isInfixOf rawName . showName . lastQName)
      -- determine pipes within named scope
      scPipes = selectFromScope
                  (\k _ -> rawName `T.isInfixOf` showName k)
                  <$> mScope
                & fromMaybe mempty
                & restrictByPartPipe inputPartPipe
      subScopes = catMaybes $ flip pipeScopeAt spc <$> subScNames
      --
      sels = (AAssembly . Runnable selInput ast <$> scPipes)
             -- So, here we're ignoring the _selected_ pipe,
             -- only collecting the _accepted_ AST.
             --
             -- Important distinction.
             <> (AScope <$> subScopes)
      --
      compR = compile opsDesc look ast

  pure $ computeFrameParams (locVal <$> ast) partPipe sels

 where
   -- | Given a set of available pipes,
   --   restrict that,
   --   by matching them against the pipe signature provided by type checker
   --   for the current input context (position inside partially resolved AST).
   restrictByPartPipe :: Maybe (PartPipe ()) -> [SomePipe ()] -> [SomePipe ()]
   restrictByPartPipe Nothing  xs = xs
   restrictByPartPipe (Just p) xs = Prelude.filter (matchMSig $ cpArgs p) xs
    where
      matchMSig :: MSig -> SomePipe () -> Bool
      matchMSig sig sp =
        -- traceErr ("verdict on " <> spname <> "::" <> show spsig <> " vs " <> show msig <> ": " <> show r)
        r
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
         tyMatch Nothing   _    = --traceErr ("match " <> spname <> ": *")
                                  True
         tyMatch (Just x) (I y) =
           traceErr ("match " <> spname <> ":" <> show x <> "|" <> show y <>
                     ": " <> show (x == y))
           $ x == y

   computeFrameParams
     :: Expr (QName Pipe)
     -> Expr (Located (PartPipe ()))
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
   somePipeSelectable (Width nameW, Width sigW) x@(AAssembly (Runnable t exp sp)) focusB =
    row $ withSomePipe sp $ \(Pipe pd _) -> do
     fixed (pure nameW) $
       richText (richTextFocusConfig (foregro V.green) focusB)
         (pure $ T.justifyRight nameW ' ' . showName $ pdName pd)
     fixed 4 $
       richText (richTextFocusConfigDef focusB)
         (pure " :: ")
     fixed (pure sigW) $
       richText (richTextFocusConfig (foregro V.blue) focusB)
         (pd & (pdSig
                >>> showSig
                >>> T.justifyLeft  sigW ' '
                >>> pure))
     pure x
   somePipeSelectable (Width nameW, Width sigW) x@(AScope scope) focusB =
    row $ do
     fixed (pure nameW) $
       richText (richTextFocusConfig (foregro V.green) focusB)
         (pure $ T.justifyRight nameW ' ' . showName $ scopeName scope)
     fixed 4 $
       richText (richTextFocusConfigDef focusB)
         (pure " :: ")
     fixed (pure sigW) $
       richText (richTextFocusConfig (foregro V.blue) focusB)
         (pure . pack . show $ scopeSize scope)
     pure x

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

-- * UI
--
 -- where
   -- escapable w = do
   --   void w
   --   i <- input
   --   return $ fforMaybe i $ \case
   --     V.EvKey V.KEsc [] -> Just Nothing
   --     _ -> Nothing

   -- look :: PipeSpace (SomePipe ()) -> QName Pipe -> Fallible (SomePipe ())
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
