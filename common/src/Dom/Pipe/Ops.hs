{-# OPTIONS_GHC -Wno-orphans #-}
module Dom.Pipe.Ops (module Dom.Pipe.Ops) where

import qualified Data.Text as T
import           Type.Reflection                    (TyCon, typeRepTyCon)

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Constr
import Dom.Pipe.IOA
import Dom.Pipe.Ops.Apply
import Dom.Pipe.Ops.Compose
import Dom.Pipe.Ops.Traverse
import Dom.Sig
import Dom.SomePipe
import Dom.SomeType
import Dom.SomeValue
import Dom.Value



-- * Operations of pipe gut composition
--
data Ops p where
  Ops ::
    { app
      :: forall l as as' o a
      . ( PipeConstr l as  o
        , PipeConstr l as' o
        , as ~ (a : as')
        )
      => Desc l as o -> Value (CTagVC a) (CTagVV a) -> p -> Fallible p
    , comp
      :: forall l vas vo fas fass ras fo
      . ( PipeConstr l vas vo
        , PipeConstr l fas fo
        , fas ~ (vo:fass)
        , ras ~ fass
        )
      => Desc l vas vo -> p -> Desc l fas fo -> p -> Fallible p
    , trav
      :: forall l fas fo a tas to
      . ( PipeConstr l fas fo
        , PipeConstr l tas to
        , fas ~ (CTagV 'Point a ': '[])
        , tas ~ '[]
        , CTagVV to ~ a
        , CTagVC fo ~ 'Point)
      => Desc l fas fo -> p -> Desc l tas to -> p -> Fallible p
    } -> Ops p

opsFull :: Ops Dynamic
opsFull = Ops
  { app  = appDyn
  , comp = compDyn
  , trav = travDyn
  }

opsDesc :: Ops ()
opsDesc = Ops
  { app  = const . const . const (Right ())
  , comp = const . const . const . const (Right ())
  , trav = const . const . const . const (Right ())
  }


-- * Processing pipe expressions
--
compile ::
     Ops p
  -> (QName Pipe -> Maybe (SomePipe p))
  -> Expr (Located (QName Pipe))
  -> PFallible (SomePipe p)
compile ops lookupPipe expr =
  resolveNames lookupPipe expr
  <&> mapLeft failMissing . locVal
  & sequence
  <&> doCompile ops
  & join
 where
   failMissing = EName . Error . ("No such pipe: " <>) . showQName

analyse ::
     (QName Pipe -> Maybe (SomePipe p))
  -> Expr (Located (QName Pipe))
  -> PFallible (Expr (Located (PartPipe p)))
analyse lookupPipe expr =
  resolveNames lookupPipe expr
  & doAnalyse


resolveNames ::
  forall f p
  . (Functor f)
  => (QName Pipe -> Maybe (SomePipe p))
  -> Expr (f (QName Pipe))
  -> Expr (f (Either (QName Pipe) (SomePipe p)))
resolveNames lookupPipe = fmap (fmap tryLookupPipe)
  where
    tryLookupPipe :: QName Pipe -> Either (QName Pipe) (SomePipe p)
    tryLookupPipe name = maybeToEither name $ lookupPipe name

doCompile :: forall p. Ops p -> Expr (SomePipe p) -> PFallible (SomePipe p)
doCompile Ops{app, comp, trav} = go
 where
   go :: Expr (SomePipe p)
      -> PFallible (SomePipe p)
   go = \case
     PVal{}      -> left EComp $ fall "doCompile:  called on a value"
     PPipe f     -> Right f
     PApp  f  x  -> goApp f x
     PComp f1 f2 -> goComp f1 f2

   goApp :: Expr (SomePipe p) -> Expr (SomePipe p) -> PFallible (SomePipe p)
   goApp PVal{}    _          = fallComp "Applying a value."
   goApp f           PVal{vX} = join $ apply app <$> go f <*> pure vX
    -- here we expect traverseP to reduce:
    --   PApp (b = IO c) (a = IO b) = (a = IO c)
   goApp f@PPipe{} x@PPipe{}  =        traverseP trav          (pP f)    (pP x)
   goApp f@PPipe{} x@PComp{}  = join $ traverseP trav <$> pure (pP f) <*> go x
   goApp f         x          = join $ traverseP trav <$> go f        <*> go x

   goComp :: Expr (SomePipe p) -> Expr (SomePipe p) -> PFallible (SomePipe p)
   goComp   PVal{}  _         = fallComp "Composing a value on the left."
   goComp _           PVal{}  = fallComp "Composing a value on the right."
   goComp l@PPipe{} r@PPipe{} = compose comp (pP l) (pP r)
   goComp l         r         = join $ compose comp <$> go l <*> go r

   fallComp = left EComp . fall

doAnalyse ::
     Expr (Located (Either (QName Pipe) (SomePipe p)))
  -> PFallible (Expr (Located (PartPipe p)))
doAnalyse expr =
  snd <$> tcExpr (TyCtx [] (Nothing, Nothing) False) expr

data TyCtx =
  TyCtx
  { tcArgs  :: [(Maybe TyCon, Maybe SomeTypeRep)]
  , tcOut   :: (Maybe TyCon, Maybe SomeTypeRep)
  , tcIsVal :: Bool
  }

tcExpr :: HasCallStack =>
     TyCtx
  -> Expr (Located (Either (QName Pipe) (SomePipe p)))
  -> PFallible (TyCtx, (Expr (Located (PartPipe p))))
tcExpr ctx expr =
  let tcArity = length (tcArgs ctx) in
  case expr of
    PVal sv@(someValueSomeType -> v) ->
      if | tcArity > 0
           -> failTc EType
           ["Expected pipe of arity ", showT tcArity, ", "
           ,"got a value: "
           , showT $ tCon v, "/", showSomeTypeRepNoKind $ tRep v]
         | otherwise -> do
             tcTy "value"
               (tcOut ctx)
               (Just $ tCon v) (Just $ tRep v)
             pure $ (,PVal sv) $
               TyCtx [] (Just $ tCon v, Just $ tRep v) True
    PPipe l@(locVal -> Left name) ->
      Right ( ctx { tcIsVal = False }
            , PPipe (l {locVal = CFreePipe (tcMSig ctx) name}) )
    PPipe l@(locVal -> Right p) ->
      let Sig{sArgs=(fmap unI -> args), sOut=(unI -> out)} = somePipeSig p
          pipeArity = length args
      in
      if | tcArity > pipeArity
           -> failTc EType
           ["Pipe '", showName (somePipeName p), "' accepts ", showT pipeArity, " "
           ,"args, but is given ", showT pipeArity]
         | pipeArity > tcArity
           -> failTc (EUnsat (somePipeName p)
                             args out
                             tcArity (drop tcArity args))
           ["Pipe '", showName (somePipeName p), "' requires ", showT pipeArity, " "
           ,"args, but is given ", showT pipeArity]
         | otherwise -> do
             tcTy ("output of pipe " <> showName (somePipeName p))
               (tcOut ctx)
               (Just $ tCon out) (Just $ tRep out)
             forM_ (zip (tcArgs ctx) (zip [1..] args)) $
               \(tcArg, (i :: Int, arg)) ->
                 tcTy ("arg "<> showT i<> " of pipe " <> showName (somePipeName p))
                      (Just $ tCon arg, Just $ tRep arg)
                      (fst tcArg) (snd tcArg)
             pure . (, PPipe (l {locVal = CSomePipe (tcMSig ctx) p})) $
               TyCtx ((Just *** Just) . (tCon &&& tRep) <$> args)
                    (Just $ tCon out, Just $ tRep out)
                    False
    PApp f x -> do
      (xCtx, eX) <- tcExpr (TyCtx [] (Nothing, Nothing) False) x
      if tcIsVal xCtx
        then do
        -- regular apply
        (fCtx, eF) <- tcExpr (ctx { tcArgs = (Nothing, Nothing) : tcArgs ctx }) f
        case (snd $ tcOut fCtx, snd $ tcOut xCtx) of
          (,) Just{} Nothing -> do
            (xCtx', eX') <- tcExpr (TyCtx [] (head $ tcArgs fCtx) False) x
            tcExpr (ctx { tcArgs = tcOut xCtx' : tcArgs ctx }) f <&>
              ((, PApp eF eX') . fst)
          (,) _ _ ->
            tcExpr (ctx { tcArgs = tcOut xCtx : tcArgs ctx }) f <&>
              ((, PApp eF eX) . fst)
        else do
        -- traverse, so we need to recheck
        when (tcArity /= 0) $
          failTc EType
            ["Pipe traversal (by ", showT f, ") "
            , "in context that supplies ", showT tcArity, " args"]
        (fCtx, eF) <- tcExpr (ctx { tcArgs = [(Just pointTyCon, snd $ tcOut xCtx)]
                                  , tcOut = (Just pointTyCon, Nothing)}) f
        (xCtx', eX') <- tcExpr (TyCtx [] (Nothing, Nothing) False) x
        pure . (,PApp eF eX') $
          TyCtx [] (fst $ tcOut xCtx', snd $ tcOut fCtx) True
    PComp f g -> do
      (gCtx, _) <- tcExpr (ctx { tcOut  = (Nothing, Nothing) }) g
      (fCtx, eF) <- tcExpr (ctx { tcArgs = [tcOut gCtx] }) f
      (_, eG) <- tcExpr (ctx { tcOut  = head $ tcArgs fCtx }) g
      pure (fCtx, PComp eF eG)
 where
   tcMSig :: TyCtx -> MSig
   tcMSig TyCtx{..} =
     Sig { sArgs = tyPairSomeType <$> tcArgs
         , sOut  = tyPairSomeType tcOut
         }
   tyPairSomeType :: (Maybe TyCon, Maybe SomeTypeRep) -> Maybe SomeType
   tyPairSomeType = \case
     (Just con, Just rep) ->
       Just CSomeType { tName = Name . showSomeTypeRepNoKind $ rep
                      , tCon  = con
                      , tRep  = rep
                      }
     _ -> Nothing
   pointTyCon = typeRepTyCon $ typeRep @Point
   tcTy :: HasCallStack => Text
        -> (Maybe TyCon, Maybe SomeTypeRep)
        -> Maybe TyCon -> Maybe SomeTypeRep
        -> PFallible ()
   tcTy desc (mCtxCon, mCtxTy) mCon mTy
       | Just ctxCon <- mCtxCon, Just con <- mCon
       , ctxCon /= con
         = failTc EKind
          ["Expected kind of ", desc, ": ", showT ctxCon, ", "
          ,"actual: ", showT con]
       | Just ctxTy <- mCtxTy, Just ty <- mTy
       , ctxTy /= ty
         = failTc EType
          ["Expected type of ", desc, ": ", showT ctxTy, ", "
          ,"actual: ", showSomeTypeRepNoKind ty]
       | otherwise -- no conflict
         = Right ()
       -- | otherwise = error $ mconcat
       --   [ "tcTy: unhandled for ", unpack desc, " (ctx/actual):  "
       --   , "con: ", show mCtxCon, "/", show mCon, ",  "
       --   , "ty: ",  show mCtxTy, "/",  show mTy
       --   ]
   failTc ctor = Left . ctor . Error . mconcat

checkPipeRunnability :: Bool -> SomePipe p -> Maybe EPipe
checkPipeRunnability remote sp
  | not $ null args
  = Just $ EUnsat (somePipeName sp)
                  args (unI . sOut $ somePipeSig sp)
                  (length args) args
                  "Not a saturated pipe"
  | remote && not (somePipeHasCap CGround sp)
  = Just . ENonGround . Error $
    "Remote execution of a Non-ground pipe: " <> showQName (somePipeQName sp)
  | otherwise =
    Nothing
 where args = fmap unI . sArgs $ somePipeSig sp

-- Represents a pipe, known or not.
data PartPipe p
  = CFreePipe
    { cpArgs  :: !MSig
    , cfpName :: !(QName Pipe)
    }
  | CSomePipe
    { cpArgs  :: !MSig
    , cspPipe :: !(SomePipe p)
    }
  deriving (Functor, Foldable, Traversable)

instance Show (PartPipe p) where
  show (CFreePipe args name) = mconcat
    [ show name
    , " :?: "
    , T.unpack $ T.intercalate " ⇨ "
      (maybe "(?)" (showSomeType False) <$> unListSig (toListSig args))]
  show (CSomePipe _args SP{..}) = unpack (showPipe spPipe)
