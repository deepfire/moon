{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Execution where

import Control.Concurrent.Async qualified      as Async
import Control.Concurrent.Chan.Unagi             (InChan, OutChan,
                                                  newChan, writeChan)
import Control.Monad.Trans.Maybe
import Data.Dynamic qualified                  as Dyn
import Data.Either qualified                   as E
import Data.IntMap qualified                   as IMap
import Data.IntMap                               (IntMap)
import Data.IORef qualified                    as IO
import Data.IORef.Extra qualified              as IO
import Data.Semigroup qualified                as S
import Data.Text qualified                     as T
import Data.String
import Data.Vector qualified                   as Vec

import Reflex                             hiding (Request)
import Reflex.Network
import Reflex.Vty                         hiding (Request)

import Data.IntUnique
import Data.Shelf

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.SomeValue
import Dom.Space.Pipe
import Dom.Value
import Dom.VTag

import Ground.Table

import Basis hiding (Dynamic)

import Reflex.SomeValue
import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Selector

import Debug.Reflex



-- * Utils
trdynt :: forall t a.
  (Reflex t, Typeable a) => (a -> String) -> Dynamic t a -> Dynamic t a
trdynt = trdynd (unpack . showTypeRepNoKind $ typeRep @a)

trevt :: forall t a.
  (Reflex t, Typeable a) => (a -> String) -> Event t a -> Event t a
trevt = trevd (unpack . showTypeRepNoKind $ typeRep @a)


data ExecutionPort t p =
  ExecutionPort
  { epPost     ::   Execution t p -> IO ()
  , epExecs    ::   OutChan (Execution t p)
  , epReplies  ::   EventSelectorInt t (PFallible SomeValue)
  , epStreamsR ::   IO.IORef (IntMap (Execution t p))
  , epSpacE    ::   Event t (PFallible (PipeSpace (SomePipe p)))
  }

mkExecutionPort ::
  forall t m p
  .  (ReflexVty t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m))
  => SomePipe p
  -> (   Execution t p
      -> Event t (PFallible (PipeSpace (SomePipe p))))
  -> Event t ()
  -> (   ExecutionPort t p
      -> (Unique -> PFallible SomeValue -> IO ())
      -> IO ())
  -> VtyWidget t m (ExecutionPort t p)
mkExecutionPort populationP selectSpaceEvents setupE portMain = mdo
  (exeSendW, epExecs) :: (InChan (Execution t p), OutChan (Execution t p)) <-
    liftIO newChan
  epStreamsR <- liftIO $ IO.newIORef mempty
  epRepliesE <- performEventAsync $
    ffor setupE \_unit (fire :: _) ->
      liftIO . (Async.link =<<) . Async.async $
        -- \exeSendR fire -> forever $ runSingleConnection tr wsa fire exeSendR
        -- fanInt :: Event t (IntMap a) -> EventSelectorInt t a
        -- selectInt :: Int -> Event t a
        portMain ep (\replyStreamId (reply :: PFallible SomeValue) -> do
                       epStreams <- IO.readIORef epStreamsR
                       maybe
                         (traceM $ mconcat
                          [ "Port error: no stream id ", show replyStreamId, ", "
                          , "known ids: ", show (IMap.keys epStreams)])
                         (const $
                          fire $ IMap.singleton (hashUnique replyStreamId) reply)
                         (IMap.lookup (hashUnique replyStreamId) epStreams))
  let epReplies = fanInt epRepliesE
      ep = ExecutionPort
           { epPost    = writeChan exeSendW
           , epSpacE   = selectSpaceEvents popExe
             -- (selectInt epReplies (hashUnique $ unHandle $ eHandle popExe))
           , ..
           }
  popExe <- makePostPipeExecution ep populationP
  pure ep

selectExecutionReplies ::
     ExecutionPort t p
  -> Execution t p
  -> Event t (PFallible SomeValue)
selectExecutionReplies ep e =
  selectInt (epReplies ep) (hashUnique $ eHandle e)

portRegisterExecution :: MonadIO m => Execution t p -> ExecutionPort t p -> m ()
portRegisterExecution e ep = liftIO $ do
  traceM $ "New execution: " <> unpack (eText e) <> ", id " <> show (eHandle e)
  IO.atomicModifyIORef'_ (epStreamsR ep)
    (IMap.insert (hashUnique $ eHandle e) e)


type MixedPipeGuts = Either () Dyn.Dynamic
type MixedPipe     = SomePipe MixedPipeGuts
type MixedPartPipe = PartPipe MixedPipeGuts
type SeparatedPipe = Either (SomePipe ()) (SomePipe Dyn.Dynamic)

data PreRunnable p
  = PreRunnable
    { prText  :: !Text
    , prExpr  :: !(Expr (Located (QName Pipe)))
    , prPExpr :: !(PFallible (Expr (Located (PartPipe p))))
    , prReq   :: !StandardRequest
    }

preRunnableText :: PreRunnable p -> Text
preRunnableText = prText

data Execution t p =
  forall c a.
  (Typeable c, Typeable a) =>
  Execution
  { eResCTag  :: (CTag c)
  , eResVTag  :: (VTag a)
  , _eText    :: Text
  -- TODO:  what does it mean for eText not to correspond to eRequest?
  , eRequest  :: StandardRequest
  , ePipe     :: (SomePipe p)
  , eReply    :: (Event t (PFallible (CapValue c a)))
  }

eText :: Execution t p -> Text
eText = _eText
{-# INLINE eText #-}

eHandle :: Execution t p -> Unique
eHandle = fst . eRequest
{-# INLINE eHandle #-}


mixedPartPipeAll ::
     (MixedPipeGuts -> Bool)
  -> Expr (Located (PartPipe MixedPipeGuts)) -> Bool
mixedPartPipeAll f =
  S.getAll . foldMap (foldMap (S.All . f) . locVal)

preRunnableAllLeft, preRunnableAllRight ::
  PreRunnable MixedPipeGuts -> Bool
preRunnableAllLeft =
  either (const False) (mixedPartPipeAll E.isLeft) . prPExpr
preRunnableAllRight =
  either (const False) (mixedPartPipeAll E.isRight) . prPExpr

mixedPipeIsT, mixedPipeIsG :: MixedPipe -> Bool
mixedPipeIsT = S.getAll . foldMap (S.All . E.isLeft)
mixedPipeIsG = S.getAll . foldMap (S.All . E.isRight)

separateMixedPipe :: MixedPipe -> SeparatedPipe
separateMixedPipe p =
  if | mixedPipeIsT p -> Left  $ fromLeft  (error "mixedPipeIsT but Right?") <$> p
     | mixedPipeIsG p -> Right $ fromRight (error "mixedPipeIsG but Left?")  <$> p
     | otherwise -> error "Mix pipe neither T nor G?"

-- XXX: this probably suboptimal, as it does selectG per call
-- XXX: potential to allow non-Ground execution
selectEvents :: (Reflex t, ReifyCTag c)
  => Event t (Either EPipe SomeValue)
  -> CTag c
  -> VTag v
  -> Event t (PFallible (CapValue c v))
selectEvents evs ctag vtag =
  unWrap <$>
  selectG (splitSVKByVTag vtag $
            -- so, we need splitSVByCTag that would thread the Left
            selectG (splitSVByCTag ctag evs)
            ctag)
          vtag

makePostPipeExecution ::
  (Reflex t, MonadIO m)
  => ExecutionPort t p
  -> SomePipe p
  -> m (Execution t p)
makePostPipeExecution ep p = do
  let req = Run . fromString . unpack . showName $ somePipeName p
  rid <- liftIO $ newUnique
  e <- mkExecution ep
         (showName $ somePipeName p)
         (rid, req)
         p
  liftIO $ epPost ep e
  pure $ e

makePostRemoteExecution :: forall t m c v.
  ( Reflex t, ReifyCTag c, MonadIO m
  , Typeable c, Typeable v, Typeable (Repr c v), HasCallStack)
  => ExecutionPort t ()
  -> CTag c
  -> VTag v
  -> Text
  -> m (Execution t MixedPipeGuts)
makePostRemoteExecution ep c v txt = do
  fmap Left <$> makePostPipeExecution ep
    (SP @() @'[] @(CTagV c v) mempty capsT $
      Pipe (mkNullaryPipeDesc (Name @Pipe txt) c v) ())

postMixedPipeRequest ::
     (Reflex t, MonadIO m)
  => ExecutionPort t ()
  -> ExecutionPort t Dyn.Dynamic
  -> Text
  -> StandardRequest
  -> SeparatedPipe
  -> m (Maybe (Execution t MixedPipeGuts))
postMixedPipeRequest epT epG txt req esp = runMaybeT $ do
  case esp of
    Left p -> do
      e <- mkExecution epT txt req p
      liftIO $ epPost epT e
      pure $ Left <$> e
    Right p -> do
      e <- mkExecution epG txt req p
      liftIO $ epPost epG e
      pure $ Right <$> e


mkExecution :: forall t p m. (MonadIO m, Reflex t)
  => ExecutionPort t p
  -> Text
  -> StandardRequest
  -> SomePipe p
  -> m (Execution t p)
mkExecution ep txt sreq@(_id, req) sp@(SP _ _ (Pipe{pDesc} :: Pipe kas o p)) =
  case req of
    Run{} ->
      mkExecution' ep (descOutCTag pDesc :: CTag (CTagVC o))
                      (descOutVTag pDesc :: VTag (CTagVV o))
    Let{} ->
      mkExecution' ep CPoint VPipeSpace
 where
   mkExecution' :: forall c v. (ReifyCTag c, Typeable c, Typeable v)
     => ExecutionPort t p
     -> CTag c
     -> VTag v
     -> m (Execution t p)
   mkExecution' ep' ctag vtag = do
     let exe = Execution ctag vtag
                         txt sreq sp
                         (selectEvents (selectExecutionReplies ep' exe) ctag vtag)
     portRegisterExecution exe ep'
     pure exe

presentExecution :: forall t m p
  . (Adjustable t m, MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m
    , NotReady t m)
  => Event t (Execution t p)
  -> VtyWidget t m (Dynamic t ())
presentExecution executionE =
  networkHold
    (boxStatic roundedBoxStyle $ text $ pure
     "You are standing at the end of a road before a small brick building.") $
     $(evl "executionReplyE" 'executionE
            [e|showQ . eText|])
      <&> \e@Execution{..} ->
      boxTitle (pure roundedBoxStyle) (" _"<>_eText<>"_ ") $
        withExecutionReply
          (pres (presentPoint "-- no data yet --"))
          (pres (presentList .
                 fmap ((Index 0, ) .
                       fromMaybe (Vec.singleton "no-Show") .
                       withCapValueShow (fmap showT))))
          (pres (presentList .
                 fmap ((Index 0, ) .
                       fromMaybe (Vec.singleton "no-Show") .
                       withCapValueShow (fmap showT))))
          (const $ text $ pure "trees not presentable yet")
          (const $ text $ pure "DAGs not presentable yet")
          (const $ text $ pure "graphs not presentable yet")
          e
 where
   pres :: (Event t (CapValue c a) -> VtyWidget t m ()) -> Event t (PFallible (CapValue c a)) -> VtyWidget t m ()
   pres f e@(fanEither -> (errE, valE)) = do
     errPrefixHeight <- holdDyn (const 0) (const 1 <$ errE)
     void $ splitV errPrefixHeight (pure (False, False))
       (richText (RichTextConfig $ pure red)
         =<< hold "" (e <&> either (const "Server error: ") (const "")))
       (text
         =<< hold "" (e <&> either (showError . unEPipe) (const "")))
     f valE
     pure ()

   -- | Present a list, with N-th element selected.
   presentList :: Event t (Index, Vector Text) -> VtyWidget t m ()
   presentList presentListXsE = mdo
     eltsD <- holdDyn mempty $ snd <$> presentListXsE
     let subsetD = (\vec (userInput, _) ->
                      Vec.filter (T.isInfixOf userInput) vec)
                   <$> eltsD
                   <*> selrInputOfftD
     Selector{..} <- selector
       SelectorParams
       { spCompletep    = \_ _ _ -> Nothing
       , spShow         = id
       , spPresent      = \focusB x ->
                            richText (richTextFocusConfigDef focusB) (pure x)
                            >> pure x
       , spElemsE       = updated subsetD
       , spInsertW      = pure ()
       , spConstituency = const True
       }
     pure ()
   presentPoint :: (MonadHold t m, Reflex t)
     => Text -> Event t (CapValue Point a) -> VtyWidget t m ()
   presentPoint defDesc e =
     text =<< hold defDesc (e <&> withCVShow showT)

withCapValueShow ::
  forall c a b
  . ReifyCTag c
  => (Show a => Repr c a -> b)
  -> CapValue c a
  -> Maybe b
withCapValueShow f CapValue{..} =
  withOpenShelf cvCaps CShow $
    f (stripValue _cvValue)

withCVShow ::
  forall c a
  . (ReifyCTag c)
  => (Show a => Repr c a -> Text)
  -> CapValue c a
  -> Text
withCVShow f cv@CapValue{..} =
  fromMaybe ("#<no-Typeable>") $
    withOpenShelf cvCaps CTypeable $
      fromMaybe ("no-Show: " <> showTypeRepNoKind (typeRep @a)) $
        withCapValueShow f cv

presentExecutionSummary ::
     forall t m p
   . (Adjustable t m, PostBuild t m, MonadNodeId m, MonadHold t m, NotReady t m, MonadFix m)
  => Execution t p
  -> VtyWidget t m ()
presentExecutionSummary =
  withExecutionReply
    (pres $ ("• " <>) . withCVShow showT)
    (pres $ ("list of " <>) . showT . length . stripValue . cvValue)
    (pres $ ("set of " <>) . showT . length . stripValue . cvValue)
    (const . text $ pure "tree")
    (const . text $ pure "dag")
    (const . text $ pure "graph")
 where
   pres :: (CapValue c a -> Text) -> Event t (PFallible (CapValue c a)) -> VtyWidget t m ()
   pres f e = do
     prefixLen <- holdDyn (const 0) (const . T.length <$> prefix)
     void $ splitH prefixLen (pure (False, False))
       (richText (RichTextConfig $ pure red)
        =<< hold "" prefix)
       (text
        =<< hold "-- no data yet --" (e <&> either (showError . unEPipe) f))
     pure ()
    where prefix = e <&> either (const "Server: ") (const "")

withExecutionReply ::
     (forall a. Event t (PFallible (CapValue Point a)) -> b)
  -> (forall a. Event t (PFallible (CapValue List  a)) -> b)
  -> (forall a. Event t (PFallible (CapValue 'Set  a)) -> b)
  -> (forall a. Event t (PFallible (CapValue Tree  a)) -> b)
  -> (forall a. Event t (PFallible (CapValue Dag   a)) -> b)
  -> (forall a. Event t (PFallible (CapValue Graph a)) -> b)
  -> Execution t p
  -> b
withExecutionReply fp fl fs ft fd fg Execution{..} =
  case eResCTag of
    CPoint -> fp eReply
    CList  -> fl eReply
    CSet   -> fs eReply
    CTree  -> ft eReply
    CDag   -> fd eReply
    CGraph -> fg eReply

instance Show (PreRunnable p) where
  show PreRunnable{..} = "#<PRERUN " <> show prExpr <> ">"

instance Functor (Execution t) where
  fmap f e@Execution{..} = e { ePipe = f <$> ePipe }
