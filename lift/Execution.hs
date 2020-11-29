module Execution where

import qualified Data.Text                              as T
import qualified Graphics.Vty                           as V

import           Reflex                            hiding (Request)
import           Reflex.Network
import           Reflex.Vty                        hiding (Request)

import Dom.CTag
import Dom.Error
import Dom.Expr
import Dom.Ground
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.SomeValue
import Dom.Value
import Dom.VTag

import Ground.Table

import qualified Wire.Protocol                          as Wire

import Basis hiding (Dynamic)

import Reflex.SomeValue
import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Selector



data Runnable p
  = Runnable
    { rText  :: !Text
    , rExpr  :: !(Expr (Located (QName Pipe)))
    , rPExpr :: !(PFallible (Expr (Located (PartPipe ()))))
    , rReq   :: !(Request (Located (QName Pipe)))
    , rPipe  :: !(p (SomePipe ()))
    }

instance Show (Runnable p) where
  show Runnable{..} = "#<RUNN " <> show rExpr <> ">"


data ExecutionPort t =
  ExecutionPort
  { epPost    :: Execution t -> IO ()
  , epReplies :: Event t (PFallible SomeValue)
  }

postExecution :: ExecutionPort t -> Execution t -> IO ()
postExecution = epPost

-- XXX: this probably suboptimal, as it does selectG per call
runnableExecution :: Reflex t
  => ExecutionPort t
  -> Runnable I
  -> Maybe (Execution t)
runnableExecution ep Runnable{..} =
  withSomeGroundPipe (unI rPipe) $
    \(Pipe{pDesc} :: Pipe Ground kas o ()) ->
      case rReq of
        Run{} ->
          let (,) ctag vtag = (descOutCTag pDesc :: CTag (TypesC o),
                               descOutVTag pDesc :: VTag (TypesV o))
          in Execution ctag vtag rText rExpr rReq $
           -- so, we need splitSVByKinds' that would thread the Left
             unWrap <$>
             selectG (splitSVKByTypes vtag $
                      selectG (splitSVByKinds ctag $ epReplies ep)
                       ctag)
             vtag
        Let{} ->
          let (,) ctag vtag = (,) TPoint VPipeSpace
          in Execution ctag vtag rText rExpr rReq $
           -- so, we need splitSVByKinds' that would thread the Left
             unWrap <$>
             selectG (splitSVKByTypes vtag $
                      selectG (splitSVByKinds ctag $ epReplies ep)
                       ctag)
             vtag

data Execution t =
  forall c a.
  (Typeable c, Typeable a, Show a) =>
  Execution
  { eResCTag  :: !(CTag c)
  , eResVTag  :: !(VTag a)
  , eText     :: !Text
  , eExpr     :: !(Expr (Located (QName Pipe)))
  -- TODO:  what does it mean for eText not to correspond to eExpr?
  --        a need for a smart constructor?
  , eRequest  :: !StandardRequest
  , eReply    :: !(Event t (PFallible (Value c a)))
  }

handleExecution ::
     (PFallible SomeValue -> IO ())
  -> Execution t
  -> PFallible Wire.Reply
  -> IO ()
handleExecution doHandle Execution{..} (Right (Wire.ReplyValue rep)) =
  case withSomeValue eResCTag eResVTag rep stripValue of
    Right{} -> doHandle (Right rep)
    Left (Error e) -> fail . unpack $
      "Server response doesn't match Execution: " <> e
handleExecution doHandle Execution{..} (Left err) = doHandle (Left err)

presentExecution :: forall t m
  . (Adjustable t m, MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m)
  => Event t (Execution t)
  -> VtyWidget t m (Dynamic t ())
presentExecution exE =
  networkHold
    (boxStatic roundedBoxStyle $ text $ pure
     "You are standing at the end of a road before a small brick building.") $
    exE <&> \e@Execution{..} ->
      boxTitle (pure roundedBoxStyle) (" _"<>eText<>"_ ") $
        withExecutionReply
          (pres (presentPoint "-- no data yet --"))
          (pres (presentList . fmap ((Index 0, ) . stripValue)))
          (pres (presentList . fmap ((Index 0, ) . stripValue)))
          (const $ text $ pure "trees not presentable yet")
          (const $ text $ pure "DAGs not presentable yet")
          (const $ text $ pure "graphs not presentable yet")
          e
 where
   pres :: (Event t (Value c a) -> VtyWidget t m ()) -> Event t (PFallible (Value c a)) -> VtyWidget t m ()
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
   presentList :: Show a => Event t (Index, [a]) -> VtyWidget t m ()
   presentList e =
     selectionMenu
       (focusButton (buttonPresentText
                      richTextFocusConfigDef
                      showT)
        >>> fmap fbFocused)
       e
       <&> pure ()
   presentPoint :: (MonadHold t m, Reflex t, Show a)
     => Text -> Event t (Value Point a) -> VtyWidget t m ()
   presentPoint def e =
     text =<< hold def (e <&> pack . show . stripValue)


presentExecutionSummary ::
     forall t m
   . (Adjustable t m, PostBuild t m, MonadNodeId m, MonadHold t m, NotReady t m, MonadFix m)
  => Execution t
  -> VtyWidget t m ()
presentExecutionSummary =
  withExecutionReply
    (pres $ ("• " <>) . showT . stripValue)
    (pres $ ("list of " <>) . showT . length . stripValue)
    (pres $ ("set of " <>) . showT . length . stripValue)
    (const . text $ pure "tree")
    (const . text $ pure "dag")
    (const . text $ pure "graph")
 where
   pres :: (Value c a -> Text) -> Event t (PFallible (Value c a)) -> VtyWidget t m ()
   pres f e = do
     prefixLen <- holdDyn (const 0) (const . T.length <$> prefix)
     splitH prefixLen (pure (False, False))
       (richText (RichTextConfig $ pure red)
        =<< hold "" prefix)
       (text
        =<< hold "-- no data yet --" (e <&> either (showError . unEPipe) f))
     pure ()
    where prefix = e <&> either (const "Server: ") (const "")

withExecutionReply ::
     (forall a. Show a => Event t (PFallible (Value Point a)) -> b)
  -> (forall a. Show a => Event t (PFallible (Value List  a)) -> b)
  -> (forall a. Show a => Event t (PFallible (Value 'Set  a)) -> b)
  -> (forall a. Show a => Event t (PFallible (Value Tree  a)) -> b)
  -> (forall a. Show a => Event t (PFallible (Value Dag   a)) -> b)
  -> (forall a. Show a => Event t (PFallible (Value Graph a)) -> b)
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
