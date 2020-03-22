module Pipe.Ops
  ( Ops(..)
  , opsFull
  , opsDesc
  -- * High-level
  , compile
  , resolveNames
  -- * Ops
  , runPipe
  , apply
  , traverseP
  -- * Constructors
  , gen, genG, gen'
  , link, linkG, link'
  , emptyDesc
  , emptyPipe
  , someEmptyPipe
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.Kind                        as K
import           Data.Maybe                         (fromJust)
import           Type.Reflection

import Basis
import Pipe.Expr
import Pipe.Ops.Apply
import Pipe.Ops.Compose
import Pipe.Ops.Traverse
import Pipe.Types
import Pipe.Zipper
import Type


-- * Operations of pipe gut composition
--
instance PipeOps Dynamic where
  pipeOps = opsFull

instance PipeOps () where
  pipeOps = opsDesc

opsFull :: Ops Dynamic
opsFull = Ops
  { app  = appDyn
  , trav = travDyn
  , comp = compDyn
  }

opsDesc :: Ops ()
opsDesc = Ops
  { app  = const . const . const ()
  , trav = const . const . const . const ()
  , comp = const . const . const . const ()
  }


-- * Compiling pipe expressions
--
compile
  :: forall m e p
  . (Monad m, e ~ Text)
  => Ops p
  -> (QName Pipe -> m (Maybe (SomePipe p)))
  -> Expr (QName Pipe)
  -> m (Either e (SomePipe p))
compile ops lookupPipe expr =
  (sequence <$> resolveNames lookupPipe expr)
    <&>
  (assemble ops <$>)
    <&>
  join . mapLeft failMissing
 where
   failMissing = ("No such pipe: " <>) . showQName

data TCR p
  = Known (SomePipe p)

checkAndInfer
  :: forall m e p
  .  (Monad m, e ~ Text)
  => Expr (Either (QName Pipe) (SomePipe p))
  -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
checkAndInfer expr = go True $ fromExpr expr
 where
   toSig :: Either (SomePipe ()) (SomePipe p) -> (SomePipe ())
   toSig (Left x)  = x
   toSig (Right p) = somePipeDesc p
   go :: Bool
      -> ZExpr (Either (QName Pipe) (SomePipe p))
      -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
   go _ (_, PVal x)          = Right (PVal x)
   go known z@(parents, PPipe p) = case p of
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
   inferFnApp n = goIFA [] where
     goIFA :: [SomePipe ()]
           -> ZExpr (Either (QName Pipe) (SomePipe p))
           -> Either e (SomePipe ())
     goIFA xs z@(Right (PApp f x):ps, self) = join $
       goIFA <$> ((:)
                  <$> undefined (go False (fromExpr x))
                  <*> pure xs)
             <*> pure (upFromLeft' z)
     goIFA xs _ = undefined xs

resolveNames
  :: forall m e p
  .  (Monad m, e ~ Text)
  => (QName Pipe -> m (Maybe (SomePipe p)))
  -> Expr (QName Pipe)
  -> m (Expr (Either (QName Pipe) (SomePipe p)))
resolveNames lookupPipe = traverse tryLookupPipe
  where
    tryLookupPipe :: QName Pipe -> m (Either (QName Pipe) (SomePipe p))
    tryLookupPipe name = maybeToEither name <$> lookupPipe name

assemble
  :: forall e p. e ~ Text
  => Ops p
  -> Expr (SomePipe p)
  -> Either e (SomePipe p)
assemble Ops{app, comp, trav} = go
  where
    go :: Expr (SomePipe p) -> Either Text (SomePipe p)
    go (PPipe p) = Right p

    go (PApp    PVal{}  _)          = Left $ "Applying a value."
    go (PApp  f           PVal{vX}) = join $ apply app <$> go f <*> pure vX
    -- here we expect traverseP to reduce:
    --   PApp (b -> IO c) (a -> IO b) -> (a -> IO c)
    go (PApp  f@PPipe{} x@PPipe{})  =        traverseP trav          (pP f)    (pP x)
    go (PApp  f@PPipe{} x@PComp{})  = join $ traverseP trav <$> pure (pP f) <*> go x
    go (PApp  f         x)          = join $ traverseP trav <$> go f        <*> go x

    go (PComp   PVal{}  _)          = Left $ "Composing a value on the left."
    go (PComp _           PVal{})   = Left $ "Composing a value on the right."
    go (PComp l@PPipe{} r@PPipe{})  = compose comp (pP l) (pP r)
    go (PComp l         r)          = join $ compose comp <$> (go l) <*> (go r)

    go PVal{} = Left "Processing a value should never happen."
