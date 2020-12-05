{-# OPTIONS_GHC -Wno-orphans #-}
module Dom.Pipe.Ops (module Dom.Pipe.Ops) where

import qualified Data.Text as T

import Basis

import Dom.CTag
import Dom.Error
import Dom.Expr
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Constr
import Dom.Pipe.Ops.Apply
import Dom.Pipe.Ops.Compose
import Dom.Pipe.Ops.Traverse
import Dom.Pipe.SomePipe
import Dom.Sig
import Dom.SomeType
import Dom.SomeValue
import Dom.Value



-- * Operations of pipe gut composition
--
data Ops p where
  Ops ::
    { app
      :: forall c cas cas' o ca
      . ( PipeConstr c cas  o
        , PipeConstr c cas' o
        , cas ~ (ca : cas')
        )
      => Desc c cas o -> Value (CTagVC ca) (CTagVV ca) -> p -> Fallible p
    , comp
      :: forall cf cv vas vo fas fass ras fo
      . ( PipeConstr cv vas vo
        , PipeConstr cf fas fo
        , fas ~ (vo:fass)
        , ras ~ fass
        )
      => Desc cv vas vo -> p -> Desc cf fas fo -> p -> Fallible p
    , trav
      :: forall cf ct fas fo a tas to
      . ( PipeConstr cf fas fo
        , PipeConstr ct tas to
        , fas ~ (CTagV 'Point a ': '[])
        , tas ~ '[]
        , CTagVV to ~ a
        , CTagVC fo ~ 'Point)
      => Desc cf fas fo -> p -> Desc ct tas to -> p -> Fallible p
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
doAnalyse = go emptyPipeCtx
 where
   emptyPipeCtx :: MSig
   emptyPipeCtx = Sig [] Nothing

   go :: MSig
      -> Expr (Located (Either (QName Pipe) (SomePipe p)))
      -> PFallible (Expr (Located (PartPipe p)))
   go (Sig args out) = \case
     PPipe (Locn l (Left pn)) -> Right . PPipe . Locn l $ CFreePipe (Sig (reverse args) out) pn
     PPipe (Locn l (Right p)) -> PPipe . Locn l <$> checkApply p (Sig (reverse args) out)

     PApp  PVal{} _           -> Left "EAnal: Applying a value."
     PApp  f      PVal{vX}    ->
       go (Sig (Just (someValueSomeType vX) : args) out) f
     PVal{vX} -> left EAnal $ fallDescShow "doAnalyse:  value handling leaked" vX
     x -> left EAnal $ fallDescShow "doAnalyse:  unhandled" x

   checkApply :: SomePipe p -> MSig -> PFallible (PartPipe p)
   checkApply p@(somePipeSig -> Sig params (I out)) args = case args of
     Sig []     Nothing    -> Right $ CSomePipe args p -- special case: unhinged..
     Sig eArgs  Nothing    -> maybeToLeft (CSomePipe args p) $
                                EAnal . Error <$> checkArgs eArgs params
     Sig eArgs (Just eRet) -> maybeToLeft (CSomePipe args p) $
                                EAnal . Error <$> checkTy "return value" p eRet out
                                  (checkArgs eArgs params)
    where
      checkArgs :: [Maybe SomeType] -> [I SomeType] -> Maybe Text
      checkArgs ctx sig = goCheck 0 ctx sig
       where
         goCheck :: Int -> [Maybe SomeType] -> [I SomeType] -> Maybe Text
         goCheck _ []    [] = Nothing -- success
         goCheck _ (_:_) [] = Just $ "Too many "   <> argcMiss (somePipeName p)
         goCheck _ [] (_:_) = Just $ "Not enough " <> argcMiss (somePipeName p)
         goCheck i (Nothing:ctx') (_:sig') =
           goCheck (i + 1) ctx' sig'
         goCheck i (Just argTy:ctx') (I paramTy:sig') =
           checkTy ("arg " <> showT i) p argTy paramTy
             (goCheck (i + 1) ctx' sig')

         argcMiss :: Name Pipe -> Text
         argcMiss name = mconcat
           [ "args for pipe \"", showT name, "\":  "
           , showT (length sig), " required, ", showT (length ctx), " passed."]
      checkTy :: Text -> SomePipe p -> SomeType -> SomeType -> Maybe Text -> Maybe Text
      checkTy desc p act exp c
        | exp == act = c
        | otherwise = Just $ mconcat
          [ "Mismatch for ", desc, " "
          , "of pipe ", showT (somePipeName p), ": "
          , "expected: ", showT exp, ", "
          , "got: ", showT act
          ]

checkPipeRunnability :: SomePipe p -> Maybe EPipe
checkPipeRunnability sp
  | not $ null args
  = Just $ EUnsat "Not a saturated pipe" args (unI . sOut $ somePipeSig sp)
  | withSomePipeGroundCase sp (const False) (const True)
  = Just $ ENonGround "Not a ground pipe"
  | otherwise = Nothing
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

instance Show (PartPipe p) where
  show (CFreePipe args name) = mconcat
    [ show name
    , " :?: "
    , T.unpack $ T.intercalate " ⇨ "
      (maybe "(?)" (showSomeType False) <$> unListSig (toListSig args))]
  show (CSomePipe args G{..}) = unpack (showPipe gPipe)
  show (CSomePipe args T{..}) = unpack (showPipe tPipe)
