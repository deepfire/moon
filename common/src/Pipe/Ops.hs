{-# OPTIONS_GHC -Wno-orphans #-}
module Pipe.Ops
  ( Ops(..)
  , opsFull
  , opsDesc
  , CPipe(..)
  -- * High-level
  , compile
  , analyse
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

import qualified Data.Text as T

import Basis
import Ground.Parser
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


-- * Processing pipe expressions
--
compile
  :: forall e p
  . (e ~ Text)
  => Ops p
  -> (QName Pipe -> Maybe (SomePipe p))
  -> Expr (Located (QName Pipe))
  -> Either e (SomePipe p)
compile ops lookupPipe expr =
  resolveNames lookupPipe expr
  <&> mapLeft failMissing . locVal
  & sequence
  <&> doCompile ops
  & join
 where
   failMissing = ("No such pipe: " <>) . showQName

analyse
  :: forall e p
  . (e ~ Text)
  => (QName Pipe -> Maybe (SomePipe p))
  -> Expr (Located (QName Pipe))
  -> Either e (Expr (Located (CPipe p)))
analyse lookupPipe expr =
  resolveNames lookupPipe expr
  & doAnalyse

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

resolveNames ::
     forall e f p
  .  (Functor f, e ~ Text)
  => (QName Pipe -> Maybe (SomePipe p))
  -> Expr (f (QName Pipe))
  -> Expr (f (Either (QName Pipe) (SomePipe p)))
resolveNames lookupPipe = fmap (fmap tryLookupPipe)
  where
    tryLookupPipe :: QName Pipe -> Either (QName Pipe) (SomePipe p)
    tryLookupPipe name = maybeToEither name $ lookupPipe name

doCompile
  :: forall e p. (e ~ Text)
  => Ops p
  -> Expr (SomePipe p)
  -> Either e (SomePipe p)
doCompile Ops{app, comp, trav} = go
 where
   go :: Expr (SomePipe p)
      -> Either Text (SomePipe p)
   go = \case
     PVal{}      -> Left "doCompile:  called on a value"
     PPipe f     -> Right f
     PApp  f  x  -> goApp f x
     PComp f1 f2 -> goComp f1 f2

   goApp :: Expr (SomePipe p) -> Expr (SomePipe p) -> Either Text (SomePipe p)
   goApp PVal{}    _          = Left $ "Applying a value."
   goApp f           PVal{vX} = join $ apply app <$> go f <*> pure vX
    -- here we expect traverseP to reduce:
    --   PApp (b = IO c) (a = IO b) = (a = IO c)
   goApp f@PPipe{} x@PPipe{}  =        traverseP trav          (pP f)    (pP x)
   goApp f@PPipe{} x@PComp{}  = join $ traverseP trav <$> pure (pP f) <*> go x
   goApp f         x          = join $ traverseP trav <$> go f        <*> go x

   goComp :: Expr (SomePipe p) -> Expr (SomePipe p) -> Either Text (SomePipe p)
   goComp   PVal{}  _         = Left $ "Composing a value on the left."
   goComp _           PVal{}  = Left $ "Composing a value on the right."
   goComp l@PPipe{} r@PPipe{} = compose comp (pP l) (pP r)
   goComp l         r         = join $ compose comp <$> go l <*> go r

doAnalyse
  :: forall e p. e ~ Text
  => Expr (Located (Either (QName Pipe) (SomePipe p)))
  -> Either e (Expr (Located (CPipe p)))
doAnalyse = go emptyPipeCtx
 where
   emptyPipeCtx :: PipeCtx
   emptyPipeCtx = PipeCtx [] Nothing

   go :: PipeCtx
      -> Expr (Located (Either (QName Pipe) (SomePipe p)))
      -> Either Text (Expr (Located (CPipe p)))
   go ctx@(PipeCtx args out) = \case
     PPipe (Locn l (Left pn)) -> Right . PPipe . Locn l $ CFreePipe pn (PipeCtx (reverse args) out)
     PPipe (Locn l (Right p)) -> PPipe . Locn l <$> checkPipeCtx p (PipeCtx (reverse args) out)

     PApp  PVal{} _           -> Left $ "Applying a value."
     PApp  f      PVal{vX}    ->
       go (PipeCtx (Just (someValueSomeType vX) : args) out) f
     PVal{vX} -> Left $ "doCompile:  " <> pack (show vX)

   checkPipeCtx :: SomePipe p -> PipeCtx -> Either e (CPipe p)
   checkPipeCtx p@(toListSig . somePipeSig -> ListSig sig) = \case
     PipeCtx [] Nothing  -> Right $ CSomePipe p -- unhinged..
     PipeCtx ctx -> checkArgs ctx sig
    where
      checkArgs :: [Maybe SomeType] -> [SomeType] -> Either e (CPipe p)
      checkArgs ctx sig = goCheck 0 ctx sig
       where
         goCheck :: Int -> [Maybe SomeType] -> [SomeType] -> Either e (CPipe p)
         goCheck _ [] (_:_) = Left $ "Too many "   <> argcMiss (somePipeName p) argC paramC
         goCheck _ (_:_) [] = Left $ "Not enough " <> argcMiss (somePipeName p) argC paramC
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

         paramC = length sig - 1
         argC   = length ctx - 1

         argcMiss :: Name Pipe -> Int -> Int -> Text
         argcMiss name _paramC _argC = mconcat
           [ "args for pipe \"", showT name, "\":  "
           , showT paramC, " required, ", showT argC, " passed."]

-- Empty list represents lack of any restraint.
-- Completely unhinged.
data PipeCtx =
  PipeCtx
  { pcArgs :: [Maybe SomeType]
  , psRet  :: Maybe SomeType
  }

-- Represents an unknown pipe.
data CPipe p
  = CFreePipe
    { cfpName :: !(QName Pipe)
    , cfpSig  :: !PipeCtx
    }
  | CSomePipe
    { cspPipe :: !(SomePipe p)
    }

instance Show (CPipe p) where
  show (CFreePipe name ctx) =
    "UnkPipe "<>(show name <>" :: "
              <>(T.unpack $ T.intercalate " ⇨  "
                  (maybe "???" (showSomeType False) <$> unPipeCtx ctx)))
  show (CSomePipe (G p))    = "GPipe "<>unpack (showPipe p)
  show (CSomePipe (T p))    = "TPipe "<>unpack (showPipe p)

showT :: Show a => a -> Text
showT = pack . show
