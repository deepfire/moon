--{-# OPTIONS_GHC -dshow-passes -dppr-debug -ddump-rn -ddump-tc #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wall -Wno-name-shadowing -Wno-unticked-promoted-constructors#-}

import qualified Control.Concurrent.Async               as Async
import           Control.Concurrent.Chan.Unagi            (InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi          as Unagi
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Control.Tracer                           (Tracer(..), traceWith)
import           Data.Char                                (isDigit)
import           Data.Semialign                           (align)
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
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.Scope
import Dom.Sig
import Dom.SomeType
import Dom.SomeValue
import Dom.Space
import Dom.Space.Pipe
import Dom.Value

import Ground.Table

import qualified Wire.Peer                              as Wire
import qualified Wire.Protocol                          as Wire

import Lift hiding (main)
import Lift.Pipe

import Reflex.SomeValue

import Execution



main :: IO ()
main = do
  sealGround -- TODO:  fix the stupid name
  mainWidget reflexVtyApp

reflexVtyApp :: forall t m.
  (MonadIO (Performable m), ReflexVty t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadIO m)
  => VtyWidget t m (Event t ())
reflexVtyApp = do
  (exeSendW, exeSendR) :: (InChan (Execution t), OutChan (Execution t)) <-
    liftIO Unagi.newChan

  (getPostBuild <&> trevs "======= reflexVtyApp: reflexVtyApp started =======")
    >>= spawnHostChatterThread stderr hostAddr exeSendR
    >>= spaceInteraction . ExecutionPort (Unagi.writeChan exeSendW)
 where
   -- XXX:  yeah, deal with that
   hostAddr = WSAddr "127.0.0.1" (cfWSPortOut defaultConfig) "/"

   spawnHostChatterThread ::
        Tracer IO Text
     -> WSAddr
     -> OutChan (Execution t)
     -> Event t ()
     -> VtyWidget t m (Event t (PFallible SomeValue))
   spawnHostChatterThread tr WSAddr{..} exeSendW postBuild =
     performEventAsync $
       ffor postBuild \_ fire -> liftIO $
         (Async.link =<<) . Async.async $
           WS.runClient wsaHost wsaPort wsaPath
             \(channelFromWebsocket -> webSockChan) ->
               void $ Wire.runClient tr webSockChan $
                 client tr fire exeSendW

data WSAddr
  = WSAddr
  { wsaHost :: String
  , wsaPort :: Int
  , wsaPath :: String
  }

client :: forall rej m a t
          . (rej ~ EPipe, m ~ IO, a ~ Wire.Reply)
       => Tracer m Text
       -> (PFallible SomeValue -> IO ())
       -> OutChan (Execution t)
       -> m (Wire.ClientState rej m a)
client tr fire reqsChan =
  go
 where
   go :: m (Wire.ClientState rej m a)
   go = do
     exe <- Unagi.readChan reqsChan
     pure . Wire.ClientRequesting (eRequest exe)
          . handleReply $ handleExecution fire exe

   handleReply
     :: (PFallible Wire.Reply -> IO ())
     -> PFallible Wire.Reply
     -> IO (Wire.ClientState rej IO Wire.Reply)
   handleReply handle x = traceWith tr' x >> handle x >> go

   tr' :: Tracer m (PFallible a)
   tr' = Tracer $ traceWith tr . presError
   presError = \case
     Left ep -> "error: " <> showError (unEPipe ep)
     Right x -> "reply: " <> pack (show x)

data Acceptable
  = APipe  !(SomePipe ())
  | AScope !(PointScope (SomePipe ()))

instance Show Acceptable where
  show x = T.unpack $ case x of
    APipe{}  -> "Pipe "  <> acceptablePresent x
    AScope{} -> "Scope " <> acceptablePresent x

acceptablePresent :: Acceptable -> Text
acceptablePresent = \case
  APipe  x -> showQName . somePipeQName $ x
  AScope x -> showName . scopeName $ x

spaceInteraction ::
     forall    t m
  .  (ReflexVty t m, PerformEvent t m, MonadIO (Performable m), MonadIO m)
  => ExecutionPort t
  -> VtyWidget t m (Event t ())
spaceInteraction ep@ExecutionPort{..} = mdo
  -- First -- ask for some space.
  liftIO $ postExecution ep $
    Execution CPoint VPipeSpace "space" "space" (Run "space") never

  let pointRepliesE :: Event t (Wrap PFallible SomeValueKinded Point)
      pointRepliesE = selectG (splitSVByCTag CPoint epReplies) CPoint
      spacePointRepliesE :: Event t (PFallible (Value Point (PipeSpace (SomePipe ()))))
      spacePointRepliesE =
        unWrap <$> selectG (splitSVKByVTag VPipeSpace pointRepliesE) VPipeSpace
  -- server -> spaceD
  spaceD :: Dynamic t (PipeSpace (SomePipe ())) <-
    holdDyn mempty (stripValue <$> snd (fanEither spacePointRepliesE))

  let (failRunnablE  :: Event t EPipe,
       maybeRunnablE :: Event t (Runnable I)) =
        fanEither (updated $ finaliseRunnable <$> fMaybeRunnableD)

  -- maybeRunnablE | filter isRight -> executionE
  executionE :: Event t (Execution t)
    <- fmap catMaybes . performEvent $
       (maybeRunnablE <&>) $
       liftIO .
       (\case
          r@Runnable{..} ->
            maybe (pure $ error "runnableExecution returned Nothing")
            (\e -> postExecution ep e >> pure (Just e)) $
              runnableExecution ep r)

  fMaybeRunnableD :: Dynamic t (PFallible (Runnable PFallible)) <-
    holdUniqDynBy eqFRunnable =<<
    mainSceneWidget
      (pipeEditorWidget spaceD
        (summaryWidget executionE failRunnablE))
      (presentExecution
        (trevs "-- spaceInteraction: new execution" executionE) >> pure ())
      (feedbackWidget
        pipesFromD
        constraintD
        fMaybeRunnableD)

  let -- runnableD -> constraintD
      constraintD :: Dynamic t (Maybe SomeTypeRep) =
        -- trdyn (\c-> mconcat ["choice: ", show c])
        fMaybeRunnableD <&>
          (fmap (rPipe
                  >>> fmap (somePipeOutSomeCTagType >>> snd)
                  >>> eitherToMaybe)
           >>> eitherToMaybe
           >>> join)

  let -- spaceD + constraintD -> pipesFromD
      pipesFromD :: Dynamic t [SomePipe ()] =
        zipDynWith pipesFromCstr spaceD constraintD
          <&> sortBy (compare `on` somePipeName)

  exitOnTheEndOf input

 where
   finaliseRunnable ::
        PFallible (Runnable PFallible)
     -> PFallible (Runnable I)
   finaliseRunnable = either Left $
     \r@Runnable{..} -> do
         pipe <- rPipe
         void $ maybeLeft checkPipeRunnability pipe
         void rPExpr
         pure r { rPipe = I pipe }

   pipeEditorWidget ::
        Dynamic t (PipeSpace (SomePipe ()))
     -> VtyWidget t m ()
     -> VtyWidget t m (Dynamic t (PFallible (Runnable PFallible)))
   pipeEditorWidget spaceD resultSummaryW = mdo
     selrWidthD :: Dynamic t Width <- fmap Width <$> displayWidth

     -- Input (selrInputOfftComplD) guides selection among pipes of the space.
     runnableAccbleD :: Dynamic t (PFallible (Runnable PFallible, [Acceptable])) <-
       pure $
         (\spc (rText, coln, _mcompld) ->
            parseGroundRequest Nothing rText
            >>= \rReq@(reqExpr -> rExpr) ->
             let rPExpr :: PFallible (Expr (Located (PartPipe ()))) =
                   analyse (lookupSomePipe spc) rExpr
             in pure (Runnable{rPipe  = compile opsDesc (lookupSomePipe spc) rExpr
                              ,..}
                     ,either (const []) id $
                      inputCompletionsForExprAndColumn spc coln rExpr <$> rPExpr))
         <$> spaceD
         <*> selrInputOfftComplD

     accbleRightsD :: Dynamic t [Acceptable] <-
       -- this ignores errors!
       holdDyn [] (fmap snd . snd . fanEither $
                   updated runnableAccbleD)

     Selector{..} <- selector
       SelectorParams
       { sfpCompletep  = completion
       , sfpShow       = acceptablePresent
       , sfpPresent    = presentAcceptable $
                           zipDyn selrWidthD accbleRightsD <&>
                             uncurry (mkPipePresentCtx " :: ")
       , sfpElemsE     = updated accbleRightsD
       , sfpInsertW    = resultSummaryW
       }

     pure $ fmap fst <$> runnableAccbleD

   summaryWidget :: (Adjustable t m, PostBuild t m, MonadNodeId m, MonadHold t m, NotReady t m, MonadFix m)
     => Event t (Execution t)
     -> Event t EPipe
     -> VtyWidget t m ()
   summaryWidget exE locErrE =
     void $ networkHold (pres red "Pipe: " $ text $ pure "-- no valid pipe --") $
       align exE locErrE <&> \case
         These _ _ -> error "summary -> align -> These exE frE -> Boom"
         This exe  -> pres blue "Result: " $ presentExecutionSummary exe
         That  err ->
           case err of
             EParse     e -> presErr red "Parse"     e
             EAnal      e -> presErr red "Analysis"  e
             EName      e -> presErr red "Name"      e
             ECompile   e -> presErr red "Compile"   e

             ENonGround e -> presErr red "NonGround" e
             EUnsat _ s o -> presUnsat "Args needed" s o

             EApply     e -> presErr red "Apply"     e
             ETrav      e -> presErr red "Traverse"  e
             EComp      e -> presErr red "Compose"   e

             EType      e -> presErr red "Type"      e
             EExec      e -> presErr red "Runtime"   e
          where
            presErr col pfx = pres col (pfx <> ": ") . text . pure . showError
            presUnsat desc args out = row $ do
              let txt col t = fixed (pure $ T.length t) $
                                richTextStatic col (pure t)
              txt red (desc <> ": ")
              txt white (head args & showSomeType False)
              forM_ (tail args) $
                (txt grey " -> " >>) . txt blue . showSomeType False
              txt grey " -> "
              txt grey (out & showSomeType False)
    where
      pres :: V.Attr -> Text -> VtyWidget t m a -> VtyWidget t m a
      pres col pref x = row $ do
        width <- displayWidth
        let len = T.length pref
        fixed (pure len) $
          richTextStatic col (pure pref)
        fixed (width <&> (\x->x-len)) x

   feedbackWidget ::
        Dynamic t [SomePipe ()]
     -> Dynamic t (Maybe SomeTypeRep)
     -> Dynamic t (PFallible (Runnable PFallible))
     -> VtyWidget t m ()
   feedbackWidget pipesFromD constraintD runnableD = do
     -- visD   <- holdDyn "" $ either (showError . unEPipe) (pack . show . snd) <$> astExpE
     -- redD   <- hold ""    $ either (showError . unEPipe) (pack . show . fst) <$> astExpE
     blueD  <- pure . current $ zipDynWith showCstrEnv constraintD pipesFromD
     redD   <- hold "" $ either (showError . unEPipe) (const "") <$> errOkE
     greenD <- hold "" $ either (const "")       (showT . rExpr) <$> errOkE

     void $ splitV (pure $ const 1) (pure $ join (,) True)
      (richTextStatic blue  blueD)
      (richTextStatic   red redD >>
       richTextStatic green greenD >> pure ())
    where
      errOkE = updated runnableD
      showCstrEnv cstr pipes =
        mconcat [ "pipes matching ",
                  fromMaybe "Nothing" $ showSomeTypeRepNoKind <$> cstr
                , ": ", pack $ show (length pipes)
                ]

   mainSceneWidget ::
        VtyWidget t m (Dynamic t (PFallible (Runnable PFallible)))
     -> VtyWidget t m ()
     -> VtyWidget t m ()
     -> VtyWidget t m (Dynamic t (PFallible (Runnable PFallible)))
   mainSceneWidget editor executionResults feedback = do
     (((runnable :: Dynamic t (PFallible (Runnable PFallible)),
        _blank), _present),
      _feedb) <-
       splitV (pure $ \x->x-8) (pure $ join (,) True)
              (splitH (pure $ flip div 2) (pure $ join (,) True)
                      (splitV (pure \x->x-1) (pure $ join (,) True)
                        editor
                        blank)
                      executionResults)
              feedback
     pure runnable

   completion :: Maybe Char -> Char -> Acceptable -> Maybe Text
   completion leftC newC acc =
     let completable =
           newC == ' ' ||
           (newC == '.' && isJust leftC
            && leftC /= Just ' '
            && fmap isDigit leftC == Just False)
     in if not completable
        then Nothing
        else Just $
             case acc of -- yes, completion is context-dependent, huh!
               AScope _ -> (<>           ".") $ acceptablePresent acc
               APipe  _ -> (<> T.pack [newC]) $ acceptablePresent acc

   eqFRunnable :: PFallible (Runnable p) -> PFallible (Runnable p) -> Bool
   eqFRunnable (Right l) (Right r) =
     rText l == rText r
   eqFRunnable _ _ = False

inputCompletionsForExprAndColumn ::
     PipeSpace (SomePipe ())
  -> Column
  -> Expr (Located (QName Pipe))
  -> Expr (Located (PartPipe ()))
  -> [Acceptable]
inputCompletionsForExprAndColumn spc (Column coln) ast partialPipe =
  -- XXX: so we replicate the same pipe.  UGH!
  (APipe <$> scPipes)
  -- So, here we're ignoring the _selected_ pipe,
  -- only collecting the _accepted_ AST.
  --
  -- Important distinction.
  <> (AScope <$> subScopes)
 where
   -- 0. map char position to scope/pipe name
   astIndex = indexLocated ast
   -- determine the completable
   inputName = fromMaybe mempty $ lookupLocated coln astIndex
   -- componentise into scope name and tail
   (scopeName, Name rawName) = unconsQName inputName &
                               fromMaybe (mempty, Name "")
   -- 1. find scope
   -- determine immediate children of named scope, matching the stem restriction
   mScope :: Maybe (PointScope (SomePipe ()))
   mScope = lookupSpaceScope (coerceQName scopeName) (psSpace spc)

   -- 2. find scope's children
   subScNames = childPipeScopeQNamesAt (coerceQName scopeName) spc
     & Prelude.filter (T.isInfixOf rawName . showName . lastQName)
   subScopes = catMaybes $ flip pipeScopeAt spc <$> subScNames

   -- 3. map char position to partial pipe in supplied expr
   indexPartPipe = indexLocated partialPipe
   inputPartPipe = lookupLocated coln indexPartPipe

   -- 4. select scope's pipes that match the provided partial pipe
   scPipes = selectFromScope
     (\k _ -> rawName `T.isInfixOf` showName k)
     <$> mScope
     & fromMaybe mempty
     & restrictByPartPipe inputPartPipe

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

presentAcceptable
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t)
  => Dynamic t PipePresentCtx
  -> Behavior t Bool
  -> Acceptable
  -> VtyWidget t m Acceptable
presentAcceptable ppcD focusB x = row (present x >> pure x)
 where
   present x = case x of
     APipe sp -> do
       let justNameLSigR = -- XXX: clearly subopt to do this here
             ppcB <&> \PipePresentCtx{..} ->
               withSomePipe sp
                 \(Pipe pd _) ->
                   (,)
                   (T.justifyRight (unWidth ppcName) ' ' . showName $ pdName pd)
                   (T.justifyLeft  (unWidth ppcSig)  ' ' . showSig  $ pdSig  pd)
       fixed (T.length . leftPad <$> ppcD) $
         richText (richTextFocusConfigDef focusB)
           (ppcB <&> leftPad)
       fixed (unWidth . ppcName <$> ppcD) $
         richText (richTextFocusConfig (foregro V.green) focusB) $
           fst <$> justNameLSigR
       fixed (T.length . ppcSep <$> ppcD) $
         richText (richTextFocusConfigDef focusB)
           (ppcB <&> ppcSep)
       fixed (unWidth . ppcSig <$> ppcD) $
         richText (richTextFocusConfig (foregro V.blue) focusB) $
           snd <$> justNameLSigR
     AScope scope -> do
       fixed (T.length . leftPad <$> ppcD) $
         richText (richTextFocusConfigDef focusB)
           (ppcB <&> leftPad)
       fixed (unWidth . ppcName <$> ppcD) $
         richText (richTextFocusConfig (foregro V.green) focusB)
           (ppcB <&> \PipePresentCtx{..} ->
             T.justifyRight (unWidth ppcName) ' ' (showName $ scopeName scope))
       fixed (T.length . ppcSep <$> ppcD) $
         richText (richTextFocusConfigDef focusB)
           (ppcB <&> ppcSep)
       fixed (unWidth . ppcSig <$> ppcD) $
         richText (richTextFocusConfig (foregro V.blue) focusB)
           (pure . pack . show $ scopeSize scope)
   ppcB = current ppcD
   leftPad PipePresentCtx{..} =
     T.replicate (unWidth ppcReserved `div` 2) " "

data PipePresentCtx =
  PipePresentCtx
  { ppcFull     :: !Width
  , ppcSep      :: !Text
  , ppcReserved :: !Width
  , ppcName     :: !Width
  , ppcSig      :: !Width
  }

mkPipePresentCtx :: Text -> Width -> [Acceptable] -> PipePresentCtx
mkPipePresentCtx sep selrWidth xs =
  PipePresentCtx { ppcSep = sep, ..}
 where
   ppcReserved = Width $ T.length sep + 2
   ppcName     = Width $ Prelude.maximum . (T.length . acceptablePresent <$>) $ xs
   ppcSig      = ppcFull - ppcReserved - ppcName
   ppcFull     = selrWidth

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
   --                 CPoint (VPoint (Name "Unit" :: Name Type)))) of
   --            Left e -> putStrLn (unpack e)
   --            Right p -> putStrLn (show p)

-- _reflexVtyApp :: forall t m.
--   (MonadIO (Performable m), ReflexVty t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadIO m)
--   => VtyWidget t m (Event t ())
-- _reflexVtyApp = mdo
--   init <- getPostBuild
--   let sp = SelectorParams
--         { sfpElemsE = init <&> const ["one", "two", "three"]
--         , sfpCompletep = charCompletionP
--         , sfpShow = id
--         , sfpPresent = \sel x ->
--             richText (richTextFocusConfig (foregro V.blue) sel) (pure x)
--             >> pure x
--         }
--   (_,
--     (Selector{..} :: Selector t m Text,
--       (_,
--          _text))) <-
--     splitV (pure \x->x-8) (pure (False, True))
--     blank $ splitV (pure $ const 5) (pure (True, False))
--     (selector sp blank) $ splitV (pure $ const 1) (pure (False, False))
--     blank $ splitV (pure $ const 1) (pure (False, False))
--     (richText (richTextFocusConfig (foregro V.green) (pure False)) (current selD))
--     (richText (richTextFocusConfig (foregro V.red)   (pure False)) (current accD))

--   selD <- ("sel: " <>) <$> holdDyn "" (snd <$> selrSelectionE)
--   accD <- ("acc: " <>) <$> holdDyn "" selrAcceptedE

--   exitOnTheEndOf input
--  where
--    charCompletionP leftC newC =
--      newC == ' ' || newC == '\t' || (newC == '.' && isJust leftC && leftC /= Just ' ')
