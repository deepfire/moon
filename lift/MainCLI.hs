--{-# OPTIONS_GHC -dshow-passes -dppr-debug -ddump-rn -ddump-tc #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wall -Wno-name-shadowing -Wno-unticked-promoted-constructors#-}

import Control.Concurrent                        (threadDelay)
import Control.Concurrent.Async qualified      as Async
import Control.Concurrent.Chan.Unagi             (InChan, OutChan)
import Control.Concurrent.Chan.Unagi qualified as Unagi
import Control.Concurrent.STM                    (TVar)
import Control.Concurrent.STM qualified        as STM
import Control.Exception                         (ErrorCall(..), Handler(..)
                                                 , SomeException (..)
                                                 , catches)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.NodeId
import Data.Char                                 (isDigit)
import Data.Dynamic qualified                  as Dyn
import Data.Monoid
import Data.Semialign                            (align)
import Data.Sequence qualified                 as Seq

import Reflex                             hiding (Request)
import Reflex.Network
import Reflex.Vty                         hiding (Request)
import Data.Text qualified                     as T
import Graphics.Vty qualified                  as V
import Network.WebSockets qualified            as WS
import System.IO.Unsafe qualified              as Unsafe

import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Selector

import Basis hiding (Dynamic)
import Debug.Reflex

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.Pipe
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.Scope
import Dom.Scope.SomePipe
import Dom.Sig
import Dom.SomeType
import Dom.SomeValue
import Dom.Space.Pipe
import Dom.Space.SomePipe
import Dom.Value

import Ground.Table

import qualified Wire.Peer                     as Wire
import qualified Wire.Protocol                 as Wire

import Lift hiding (main)
import Lift.Pipe
import Execution

import Reflex.SomeValue



main :: IO ()
main = withDotTracing "trace.dot" $ do
  traceM $ pave '-' <> pave '=' <> " main " <> pave '=' <> pave '-'
  sealGround -- TODO:  fix the stupid name
  mainWidget reflexVtyApp

pave :: Char -> String
pave c = take 21 $ repeat c

reflexVtyApp :: forall t m.
  (MonadIO (Performable m), ReflexVty t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadIO m)
  => VtyWidget t m (Event t ())
reflexVtyApp = do
  setupE <- getPostBuild <&> trevs
    ("// " <> pave '-' <> " reflexVtyApp: reflexVtyApp started " <> pave '-')
  r <- mkRemoteExecutionPort stderr hostAddr setupE
  l <- mkLocalExecutionPort stderr setupE
  spaceInteraction r l
 where
   -- XXX:  yeah, deal with that
   hostAddr = WSAddr "127.0.0.1" (cfWSPortOut defaultConfig) "/"

mkExecutionPort ::
  forall t m p
  .  (ReflexVty t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m))
  => (Event t (PFallible SomeValue)
      -> Event t (PFallible (PipeSpace (SomePipe p))))
  -> (OutChan (Execution t p) -> (PFallible SomeValue -> IO ()) -> IO ())
  -> Event t ()
  -> VtyWidget t m (ExecutionPort t p)
mkExecutionPort selectSpaceEvents handler setupE = do
  (exeSendW, exeSendR) :: (InChan (Execution t p), OutChan (Execution t p)) <-
    liftIO Unagi.newChan

  evs <- performEventAsync $
    ffor setupE \_ fire -> liftIO $
      (Async.link =<<) . Async.async $
        handler exeSendR (\e ->
                            fire $
                            -- traceErr ("...\nA space has arrived: " <> show e
                            --           <> "\ndigraph events {")
                            e)

  pure ExecutionPort
    { epPost    = Unagi.writeChan exeSendW
    , epReplies = evs
    , epSpacE   = selectSpaceEvents evs
    }

pointRepliesE :: Reflex t => Event t (PFallible SomeValue) -> Event t (Wrap PFallible SomeValueKinded Point)
pointRepliesE evs = selectG (splitSVByCTag CPoint evs) CPoint

mkRemoteExecutionPort ::
  forall t m
  . (ReflexVty t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m))
  => Tracer IO Text
  -> WSAddr
  -> Event t ()
  -> VtyWidget t m (ExecutionPort t ())
mkRemoteExecutionPort tr wsa =
  mkExecutionPort spacePointRepliesE $
    \exeSendR fire -> forever $ do
      catches (runSingleConnection tr wsa fire exeSendR)
        [ Handler (\(ErrorCall e) ->
                     fire . Left . EExec . Error $
                       showT e)
        , Handler (\(SomeException e) ->
                     fire . Left . EExec . Error $
                       "Uncaught SomeException: " <> showT e)
        ]
 where
   runSingleConnection ::
          Tracer IO Text
       -> WSAddr
       -> (PFallible SomeValue -> IO ())
       -> OutChan (Execution t ())
       -> IO ()
   runSingleConnection tr WSAddr{..} fire exeSendR =
     WS.runClient wsaHost wsaPort wsaPath
       \(channelFromWebsocket -> webSockChan) ->
         void $ Wire.runClient (Tracer $ const $ pure ()) webSockChan $
           client tr fire exeSendR

   spacePointRepliesE :: Event t (PFallible SomeValue) -> Event t (PFallible (PipeSpace (SomePipe ())))
   spacePointRepliesE evs =
     fmap (stripValue . cvValue) . unWrap <$> selectG (splitSVKByVTag VPipeSpace (pointRepliesE evs)) VPipeSpace

mkLocalExecutionPort ::
  forall t m
  . (ReflexVty t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m))
  => Tracer IO Text
  -> Event t ()
  -> VtyWidget t m (ExecutionPort t Dyn.Dynamic)
mkLocalExecutionPort _tr =
  mkExecutionPort spacePointRepliesE $
    \exeSendR fire -> do
      threadDelay 1000000
      fire =<< (left EExec <$> runSomePipe localSpacePipe)
      forever $ do
        Execution{..} <- Unagi.readChan exeSendR
        fire =<< (left EExec <$> runSomePipe ePipe)
 where
   spacePointRepliesE :: Event t (PFallible SomeValue) -> Event t (PFallible (PipeSpace (SomePipe Dyn.Dynamic)))
   spacePointRepliesE evs =
     join . fmap splitPipeSpaces <$> evs
   splitPipeSpaces :: SomeValue -> PFallible (PipeSpace (SomePipe Dyn.Dynamic))
   splitPipeSpaces sv =
     case withExpectedSomeValue' CPoint (Proxy @(PipeSpace (SomePipe Dyn.Dynamic))) sv id of
       Left  e -> Left $ EExec $ traceErr (unpack $ showError e) e
       Right x -> Right $ stripValue x

localPipeSpace :: TVar (SomePipeSpace Dyn.Dynamic)
localPipeSpace = Unsafe.unsafePerformIO $ STM.newTVarIO Main.initialPipeSpace
{-# NOINLINE localPipeSpace #-}

getLocalPipeSpace :: IO (PipeSpace (SomePipe Dyn.Dynamic))
getLocalPipeSpace = STM.readTVarIO localPipeSpace

localSpacePipe :: SomePipe Dyn.Dynamic
localSpacePipe =
  somePipe0  "local-space" capsTS CVPoint (Right <$> getLocalPipeSpace)

initialPipeSpace :: SomePipeSpace Dyn.Dynamic
initialPipeSpace
  = emptySomePipeSpace "Root"
    & spsInsertScopeAt mempty
      (pipeScope ""
       -- Generators:
       --
        [ localSpacePipe
        ])

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
       -> OutChan (Execution t ())
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
   handleReply handle x =
     either (traceWith tr') (const $ pure ()) x >> handle x >> go

   tr' :: Tracer m EPipe
   tr' = contramap (showError . unEPipe) tr

data Acceptable
  = APipe  !MixedPipe
  | AScope !(PointScope MixedPipe)

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
  .  (ReflexVty t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadIO m)
  => ExecutionPort t ()
  -> ExecutionPort t Dyn.Dynamic
  -> VtyWidget t m (Event t ())
spaceInteraction epRemote epLocal = mdo
  void $ liftIO $ postExecution epRemote $ -- Request remote's pipe space.
    Execution @t @() CPoint VPipeSpace "space" (Run "space")
      (SP @() @'[] @(CTagV Point ()) mempty capsTSG $
       Pipe undefined undefined) never

  -- server + local -> spaceD
  let (,) remoteSpaceErrsE remoteSpaceE = fanEither $ epSpacE epRemote
      -- XXX: local space disabled
      (,) localSpaceErrsE   localSpaceE = fanEither $ never --epSpacE epLocal
  remoteSpaceD :: Dynamic t (PipeSpace (SomePipe ())) <-
    holdDyn mempty remoteSpaceE
  localSpaceD :: Dynamic t (PipeSpace (SomePipe Dyn.Dynamic)) <-
    holdDyn mempty localSpaceE
  let spaceD :: Dynamic t (PipeSpace MixedPipe)
      spaceD = zipDynWith (<>)
                 ($(dev "spaceD" 'localSpaceD)  <&> fmap (fmap Right))
                 ($(dev "spaceD" 'remoteSpaceD) <&> fmap (fmap Left))

  fRunnableD ::
    Dynamic t (PFallible (PreRunnable MixedPipeGuts, SeparatedPipe))
    <- pure $ compilePreRunnable
               <$> trdevs "remoteSpaceD -> fRunnableD;" remoteSpaceD
               <*> trdevs "localSpaceD -> fRunnableD;" localSpaceD
               <*> trdevs "fPreRunnableD -> fRunnableD;" fPreRunnableD

  let compilePreRunnable ::
           SomePipeSpace ()
        -> SomePipeSpace Dyn.Dynamic
        -> PFallible (PreRunnable MixedPipeGuts)
        -> PFallible (PreRunnable MixedPipeGuts, SeparatedPipe)
      compilePreRunnable spcRem spcLoc fpr = do
         pr@PreRunnable{..} <- fpr
         _pexp <- prPExpr
         p <- if | preRunnableAllLeft pr
                 -> fmap Left  <$> compile opsDesc (lookupSomePipe spcRem) prExpr
                 | preRunnableAllRight pr
                 -> fmap Right <$> compile opsFull (lookupSomePipe spcLoc) prExpr
                 | otherwise ->
                   Left . ECompile . Error $ "Inconsistent pipe locality: " <> showT prExpr
         traceM . mconcat $
           [ "preRunnable: ", show pr, ", "
           , "allLeft: ", show $ preRunnableAllLeft pr, ", "
           , "has CGround: ", show $ somePipeHasCap CGround p, ", "
           , "runnability issues: ", show $ checkPipeRunnability
                                              (preRunnableAllLeft pr) p, ", "
           ]
         void $ maybeLeft (checkPipeRunnability
                           $ (\x -> flip trace x $
                               "preRunnableAllLeft: " <> show x <> " " <> show p)
                           $ preRunnableAllLeft pr) p
         pure (pr, separateMixedPipe p)

      -- 1. Separate the syntactically valid requests from chaff.
      (runnableFailE :: Event t EPipe,
       runnablE  :: Event t (PreRunnable MixedPipeGuts, SeparatedPipe)) =
        (trevs "fRunnableD -> runnableFailE;" {- show -} ***
         trevs "fRunnableD -> runnablE;"      {- (unpack . prText . fst) -}) $
        fanEither (updated fRunnableD)

      nextArgTypeE :: Event t (Maybe SomeTypeRep)
      nextArgTypeE = trevs "runnableFailE -> nextArgTypeE;"
                     runnableFailE
                       <&> \case
                             EUnsat _ _ _ _ (x:_) _ -> Just $ tRep x
                             _ -> Nothing
      errorsE :: Event t EPipe
      errorsE =
        leftmost
        [ trevs "runnableFailE -> errorsE;" runnableFailE
        , trevs "remoteSpaceErrsE -> errorsE;" remoteSpaceErrsE
        , trevs "localSpaceErrsE -> errorsE;" localSpaceErrsE
        ]

  -- spaceE + runnablE -> executionE
  executionE ::
    Event t (Execution t MixedPipeGuts)
    <- performEvent $
        (attachPromptlyDyn
           $(dev "executionE" 'spaceD)
           $(ev  "executionE" 'runnablE)
           <&>) $
        liftIO .
        -- XXX: WTF?
        (\(_spc, (PreRunnable{..}, p)) ->
           maybe (error "postMixedPipeRequest returned Nothing")
             id <$>
             (postMixedPipeRequest epRemote epLocal prText prReq p)
           )

  (constraintE, fireConstraintE) <- newTriggerEvent
  constraintD <-
    holdDyn Nothing constraintE

  -- 0. UI provides syntactically valid requests, and also
  --    displays results of validation.
  fPreRunnableD ::
    Dynamic t (PFallible (PreRunnable MixedPipeGuts)) <-
    mainSceneWidget
      (pipeEditorWidget spaceD constraintD
        (summaryWidget
          $(ev "summaryWidget" 'executionE)
          (trevs "errorsE -> summaryWidget;" errorsE)))
      (presentExecution
       $(ev "presentExecution" 'executionE)
       >> pure ())
      (feedbackWidget
        (trdevs "pipesFromD -> feedbackWidget;" pipesFromD)
        (trdevs "constraintD -> feedbackWidget;" constraintD)
        (fmap fst <$> fRunnableD))

  void . performEvent $
    trevs "nextArgTypeE -> constraintD;"
    nextArgTypeE <&>
      liftIO . fireConstraintE

  let -- spaceD + constraintD -> pipesFromD
      pipesFromD :: Dynamic t [MixedPipe] =
        zipDynWith pipesToCstr
          (trdevs "spaceD -> pipesFromD;" spaceD)
          (trdevs "constraintD -> pipesFromD;" constraintD)
          <&> sortBy (compare `on` somePipeName)

  exitOnTheEndOf input

 where
   pipeEditorWidget ::
        Dynamic t (PipeSpace MixedPipe)
     -> Dynamic t (Maybe SomeTypeRep)
     -> VtyWidget t m ()
     -> VtyWidget t m (Dynamic t (PFallible (PreRunnable MixedPipeGuts)))
   pipeEditorWidget spaceD constraintD resultSummaryW = mdo
     selrWidthD :: Dynamic t Width <- fmap Width <$> displayWidth

     -- Input (selrInputOfftComplD) guides selection among pipes of the space.
     reqAnalysedAccbleD ::
       Dynamic t
       (PFallible ( PreRunnable MixedPipeGuts
                  , [Acceptable])) <-
       pure $
         (\spc (prText, coln@(Column intColn)) constr ->
            parseGroundRequest (Just intColn) prText
            >>= \prReq@(reqExpr -> prExpr) -> pure
             let prPExpr = analyse (lookupSomePipe spc) prExpr
             in  ( PreRunnable{..}
                 , either (const []) id $
                   let r = case prPExpr of
                             Right pExp -> Right (constr, pExp)
                             Left u@EUnsat{} ->
                               Right (Just $ tRep $ head $ epMiss u, undefined)
                             Left e -> Left e
                   in r <&>
                      inputCompletionsForExprAndColumn spc coln prExpr))
         <$> trdevs "spaceD -> reqAnalysedAccbleD;"
             spaceD
         <*> trdevs "selrInputOfftD -> reqAnalysedAccbleD;"
             selrInputOfftD
         <*> trdevs "constraintD -> reqAnalysedAccbleD;"
             constraintD

     accbleRightsD :: Dynamic t [Acceptable] <-
       -- this ignores errors!
       holdDyn [] (fmap snd . snd . fanEither . updated $
                   $(dev "accbleRightsD" 'reqAnalysedAccbleD))

     Selector{..} <- selector
       SelectorParams
       { spCompletep    = completion
       , spShow         = acceptablePresent
       , spPresent      = presentAcceptable $
                            zipDyn
                              selrWidthD
                              (trdevs
                               "accbleRightsD -> spPresent;"
                               accbleRightsD)
                              <&> uncurry (mkPipePresentCtx " :: ")
       , spElemsE       = trevs "accbleRightsD -> spElemsE;" $
                          updated accbleRightsD
       , spInsertW      = resultSummaryW
       , spConstituency = Dom.Name.nameConstituent
       }
     holdUniqDynBy
        eqFPreRunnable
        ($(dev "fPreRunnableD" 'reqAnalysedAccbleD)
         <&> fmap fst)

   eqFPreRunnable :: PFallible (PreRunnable MixedPipeGuts) -> PFallible (PreRunnable MixedPipeGuts) -> Bool
   eqFPreRunnable (Right l) (Right r) = ((==) `on` prText) l r
   eqFPreRunnable _ _ = False

   summaryWidget :: (Adjustable t m, PostBuild t m, MonadNodeId m, MonadHold t m, NotReady t m, MonadFix m)
     => Event t (Execution t MixedPipeGuts)
     -> Event t EPipe
     -> VtyWidget t m ()
   summaryWidget exE errorsE =
     void $ networkHold (pres red "Pipe: " $ text $ pure "-- no valid pipe --") $
       align exE errorsE <&> \case
         These _ _ -> error "summary -> align -> These exE frE -> Boom"
         This exe  -> pres blue "Result: " $ presentExecutionSummary exe
         That  err ->
           case err of
             EParse     e -> presErr red "Parse"     e
             EAnal      e -> presErr red "Analysis"  e
             EName      e -> presErr red "Name"      e
             ECompile   e -> presErr red "Compile"   e

             ENonGround e -> presErr red "NonGround" e
             EUnsat name s o done _ _ ->
                             presUnsat (showName name) s o done

             EApply     e -> presErr red "Apply"     e
             ETrav      e -> presErr red "Traverse"  e
             EComp      e -> presErr red "Compose"   e

             EType      e -> presErr red "Type"      e
             EKind      e -> presErr red "Kind"      e
             EExec      e -> presErr red "Runtime"   e
          where
            presErr col pfx = pres col (pfx <> ": ") . text . pure . showError
            presUnsat desc args out done = row $ do
              let txt col t = fixed (pure $ T.length t) $
                                richTextStatic col (pure t)
              txt yellow desc
              txt grey (" :: ")
              let (doneArgs, curArg:unsatArgs) = splitAt done args
              forM_ doneArgs $
                (>> txt grey " -> ") . txt blue . showSomeType False
              txt white (curArg & showSomeType False)
              txt grey " -> "
              forM_ unsatArgs $
                (>> txt grey " -> ") . txt red . showSomeType False
              txt green (out & showSomeType False)
    where
      pres :: V.Attr -> Text -> VtyWidget t m a -> VtyWidget t m a
      pres col pref x = row $ do
        width <- displayWidth
        let len = T.length pref
        fixed (pure len) $
          richTextStatic col (pure pref)
        fixed (width <&> (\x->x-len)) x

   feedbackWidget ::
        Dynamic t [MixedPipe]
     -> Dynamic t (Maybe SomeTypeRep)
     -> Dynamic t (PFallible (PreRunnable MixedPipeGuts))
     -> VtyWidget t m ()
   feedbackWidget pipesFromD constraintD preRunD = do
     -- visD   <- holdDyn "" $ either (showError . unEPipe) (pack . show . snd) <$> astExpE
     -- redD   <- hold ""    $ either (showError . unEPipe) (pack . show . fst) <$> astExpE
     blueD  <- pure . current $ zipDynWith showCstrEnv constraintD pipesFromD
     redD   <- hold "" $ either (showError . unEPipe) (const "") <$> errOkE
     greenD <- hold "" $ either (const "")      (showT . prExpr) <$> errOkE

     void $ splitV (pure $ const 1) (pure $ join (,) True)
      (richTextStatic blue  blueD)
      (richTextStatic   red redD >>
       richTextStatic green greenD >> pure ())
    where
      errOkE = updated preRunD
      showCstrEnv cstr pipes =
        mconcat [ "Pipes producing ",
                  fromMaybe "Nothing" $ showSomeTypeRepNoKind <$> cstr
                , ": ", pack $ show (length pipes)
                ]

   mainSceneWidget ::
        VtyWidget t m (Dynamic t a)
     -> VtyWidget t m ()
     -> VtyWidget t m ()
     -> VtyWidget t m (Dynamic t a)
   mainSceneWidget editor executionResults feedback = do
     (((runnable,
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
           (\x ->
              -- trace
              -- (mconcat
              --  [ "completion: leftC=", catMaybes [leftC], ", "
              --  , "newC=", [newC], ", "
              --  , "acc=", show acc, ", "
              --  , "res=", show x
              --  ])
              x
           ) $
     let completable =
           (leftC <&>
            \x-> (Dom.Name.nameConstituent x || x == '.') &&
                 not (isDigit x)) == Just True &&
           (newC == ' ' ||
            newC == '.')
     in if not completable
        then Nothing
        else Just $
             case acc of -- yes, completion is context-dependent, huh!
               AScope _ -> (<>           ".") $ acceptablePresent acc
               APipe  _ -> (<> T.pack [newC]) $ acceptablePresent acc

inputCompletionsForExprAndColumn ::
     PipeSpace MixedPipe
  -> Column
  -> Expr (Located (QName Pipe))
  -> (Maybe SomeTypeRep, Expr (Located MixedPartPipe))
  -> [Acceptable]
inputCompletionsForExprAndColumn spc (Column coln) ast (constr, _partialPipe) =
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
   inputName@(QName iqn) = fromMaybe mempty $ lookupLocated coln astIndex
   -- componentise into scope name and tail
   (scopeName, Name rawName) = unsnocQName inputName &
                               fromMaybe (mempty, Name "")
   -- 1. find scope
   -- determine immediate children of named scope, matching the stem restriction
   -- mScope :: Maybe (PointScope MixedPipe)
   -- mScope = lookupSpaceScope (coerceQName scopeName) (psSpace spc)

   -- 2. find scope's children
   subScNames = childPipeScopeQNamesAt (coerceQName scopeName) spc
     & Prelude.filter (T.isInfixOf rawName . showName . lastQName)
   subScopes = catMaybes $ flip pipeScopeAt spc <$> subScNames

   -- 3. map char position to partial pipe in supplied expr
   -- indexPartPipe = indexLocated partialPipe
   -- inputPartPipe = lookupLocated coln indexPartPipe

   -- 4. select scope's pipes that match the provided partial pipe
   scPipes =
     pipesToCstr spc
     (-- trace
      --  (mconcat
      --   [ "COMPLETION: "
      --   , "constr=", show constr, ", "
      --   , "input: '", show inputName, "'"
      --   ])
      constr)
     & case constr of
         Just{} ->
           filter (spQName >>>
                    \(QName (_ :|> x)) ->
                      (rawName `T.isInfixOf`) $ showName x)
         Nothing ->
           filter (spQName >>>
                    \(QName spqn) -> getAll $
                      bifoldSeq (\(Name x) (Name y)-> All $ x `T.isInfixOf` y)
                        (All True) (All $ Seq.length spqn == 1 && Seq.length iqn < 2)
                        iqn spqn)

   -- scPipes = selectFromScope
   --   (\k _ -> rawName `T.isInfixOf` showName k)
   --   <$> mScope
   --   & fromMaybe mempty
   --   & restrictByPartPipe inputPartPipe
bifoldSeq :: Monoid m => (a -> b -> m) -> m -> m -> Seq a -> Seq b -> m
bifoldSeq f z m xs' ys' = go xs' ys' z
  where
    go Seq.Empty  Seq.Empty  acc = acc
    go Seq.Empty  _          _   = m
    go _          Seq.Empty  _   = m
    go (x :<| xs) (y :<| ys) acc = go xs ys (acc <> f x y)

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
