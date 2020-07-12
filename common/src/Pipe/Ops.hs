module Pipe.Ops
  ( Ops(..)
  , opsFull
  , opsDesc
  -- * High-level
  , compile
  , resolveNames
  -- unused
  , checkAndInfer
  -- * Ops
  , runPipe
  , apply
  , compose
  , traverseP
  -- * Constructors
  , gen, genG, gen'
  , link, linkG, link'
  -- , emptyDesc
  -- , emptyPipe
  -- , someEmptyPipe
  )
where

import Basis
import Pipe.Expr
import Pipe.Ops.Base
import Pipe.Ops.Apply
import Pipe.Ops.Compose
import Pipe.Ops.Traverse
import Pipe.Types
import Pipe.Zipper
import Type
import SomeType


-- * Operations of pipe gut composition
--
instance PipeOps Dynamic where
  pipeOps = opsFull

instance PipeOps () where
  pipeOps = opsDesc

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


-- * Compiling pipe expressions
--
compile
  :: forall e p
  . (e ~ Text)
  => Ops p
  -> (QName Pipe -> Maybe (SomePipe p))
  -> Expr (QName Pipe)
  -> Either e (SomePipe p)
compile ops lookupPipe expr = join $
  doCompile ops (resolveNames lookupPipe expr)
    <&> unCPipeFailUnknown

checkAndInfer
  :: forall m e p
  .  (Monad m, e ~ Text)
  => Expr (Either (QName Pipe) (SomePipe p))
  -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
checkAndInfer expr = go True $ fromExpr expr
 where
   -- toSig :: Either (SomePipe ()) (SomePipe p) -> SomePipe ()
   -- toSig (Left x)  = x
   -- toSig (Right p) = somePipeDesc p
   go :: Bool
      -> ZExpr (Either (QName Pipe) (SomePipe p))
      -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
   go _ (_, PVal x)          = Right (PVal x)
   go _known z@(parents, PPipe p) = case p of
     Right x   -> Right . PPipe $ Right x
     Left name -> case parents of
       [] -> Left $ "Lacking context to infer unknown: " <> showQName name
       Right PApp{}:_  -> PPipe . Left <$> inferFnApp name z
       Left  PApp{}:_  -> undefined
       Right PComp{}:_ -> undefined
       Left  PComp{}:_ -> undefined
       _ -> Left $ "Child of an atom: " <> showQName name
   inferFnApp
     :: QName Pipe
     -> ZExpr (Either (QName Pipe) (SomePipe p))
     -> Either e (SomePipe ())
   inferFnApp _n = goIFA [] where
     goIFA :: [SomePipe ()]
           -> ZExpr (Either (QName Pipe) (SomePipe p))
           -> Either e (SomePipe ())
     goIFA xs z@(Right (PApp _f x):_ps, _self) = join $
       goIFA <$> ((:)
                  <$> undefined (go False (fromExpr x))
                  <*> pure xs)
             <*> pure (upFromLeft' z)
     goIFA xs _ = undefined xs

resolveNames
  :: forall e p
  .  (e ~ Text)
  => (QName Pipe -> Maybe (SomePipe p))
  -> Expr (QName Pipe)
  -> Expr (Either (QName Pipe) (SomePipe p))
resolveNames lookupPipe = fmap tryLookupPipe
  where
    tryLookupPipe :: QName Pipe -> Either (QName Pipe) (SomePipe p)
    tryLookupPipe name = maybeToEither name $ lookupPipe name

doCompile
  :: forall e p. e ~ Text
  => Ops p
  -> Expr (Either (QName Pipe) (SomePipe p))
  -> Either e (CPipe p)
doCompile Ops{app, comp, trav} = go emptyPipeCtx
 where
   emptyPipeCtx :: PipeCtx
   emptyPipeCtx = PipeCtx []

   go :: PipeCtx
      -> Expr (Either (QName Pipe) (SomePipe p))
      -> Either Text (CPipe p)
   go ctx = \case
     PPipe (Left pn)            -> Right $ CFreePipe pn ctx
     PPipe (Right p)            -> checkPipeCtx p ctx
     PVal{} -> Left "doCompile:  fall-through"

     -- PApp    PVal{}  _          -> Left $ "Applying a value."
     -- PApp  f           PVal{vX} -> join $ apply app <$> go f <*> pure vX
     -- -- here we expect traverseP to reduce:
     -- --   PApp (b -> IO c) (a -> IO b) -> (a -> IO c)
     -- PApp  f@PPipe{} x@PPipe{}  ->        traverseP trav          (pP f)    (pP x)
     -- PApp  f@PPipe{} x@PComp{}  -> join $ traverseP trav <$> pure (pP f) <*> go x
     -- PApp  f         x          -> join $ traverseP trav <$> go f        <*> go x

     -- PComp   PVal{}  _          -> Left $ "Composing a value on the left."
     -- PComp _           PVal{}   -> Left $ "Composing a value on the right."
     -- PComp l@PPipe{} r@PPipe{}  -> compose comp (pP l) (pP r)
     -- PComp l         r          -> join $ compose comp <$> go l <*> go r

     -- PVal{} -> Left "doCompile:  called on a value"

   checkPipeCtx :: SomePipe p -> PipeCtx -> Either e (CPipe p)
   checkPipeCtx p@(toListSig . somePipeSig -> ListSig pipeSig) = \case
     PipeCtx []  -> Right $ CSomePipe p -- unhinged..
     PipeCtx ctx -> checkArgs ctx pipeSig
    where
      checkArgs :: [Maybe SomeType] -> [SomeType] -> Either e (CPipe p)
      checkArgs ctx pipeSig = goCheck 0 ctx pipeSig
       where
         goCheck :: Int -> [Maybe SomeType] -> [SomeType] -> Either e (CPipe p)
         goCheck _ [] (_:_) = Left $ "Not enough " <> argcMiss (somePipeName p) argC paramC
         goCheck _ (_:_) [] = Left $ "Too many "   <> argcMiss (somePipeName p) argC paramC
         goCheck i (Nothing:ctx') (_:sig') =
           goCheck (i + 1) ctx' sig'
         goCheck i (Just ctxTy:ctx') (argTy:sig') =
           if ctxTy == argTy
           then goCheck (i + 1) ctx' sig'
           else Left $ mconcat
                [ "Mismatch for arg ", showT i, " "
                , "of pipe ", showT (somePipeName p), ": "
                , "expected: ", showT ctxTy, ", "
                , "got: ", showT argTy]

         paramC = length pipeSig - 1
         argC   = length ctx     - 1

         argcMiss :: Name Pipe -> Int -> Int -> Text
         argcMiss name paramC argC = mconcat
           [ "args for pipe \"", showT name, "\":  "
           , showT paramC, " required, ", showT argC, " passed."]

-- Empty list represents lack of any restraint.
-- Completely unhinged.
newtype PipeCtx = PipeCtx { unPipeCtx :: [Maybe SomeType] }

-- Represents an unknown pipe.
data CPipe p
  = CFreePipe
    { cfpName :: !(QName Pipe)
    , cfpSig  :: !PipeCtx
    }
  | CSomePipe
    { cspPipe :: !(SomePipe p)
    }

unCPipeFailUnknown :: CPipe p -> Either Text (SomePipe p)
unCPipeFailUnknown (CSomePipe p) = Right p
unCPipeFailUnknown CFreePipe{..} = Left $ mconcat
  [ "Unknown pipe: ", showT cfpName ]

showT = pack . show
