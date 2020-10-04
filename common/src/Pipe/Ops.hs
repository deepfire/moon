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
import Pipe.Expr
import Pipe.Ops.Base
import Pipe.Ops.Apply
import Pipe.Ops.Compose
import Pipe.Ops.Traverse
import Pipe.Types
import Pipe.Zipper
import Type


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
      => Desc c cas o -> Value (CTagOf ca) (TypeOf ca) -> p -> Either Text p
    , comp
      :: forall cf cv vas vo fas fass ras fo
      . ( PipeConstr cv vas vo
        , PipeConstr cf fas fo
        , fas ~ (vo:fass)
        , ras ~ fass
        )
      => Desc cv vas vo -> p -> Desc cf fas fo -> p -> Either Text p
    , trav
      :: forall cf ct fas fo a tas to
      . ( PipeConstr cf fas fo
        , PipeConstr ct tas to
        , fas ~ (Type 'Point a ': '[])
        , tas ~ '[]
        , TypeOf to ~ a
        , CTagOf fo ~ 'Point)
      => Desc cf fas fo -> p -> Desc ct tas to -> p -> Either Text p
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
  forall e p
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

analyse ::
  forall e p
  . (e ~ Text)
  => (QName Pipe -> Maybe (SomePipe p))
  -> Expr (Located (QName Pipe))
  -> Either e (Expr (Located (CPipe p)))
analyse lookupPipe expr =
  resolveNames lookupPipe expr
  & doAnalyse

checkAndInfer ::
  forall m e p
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
  . (Functor f, e ~ Text)
  => (QName Pipe -> Maybe (SomePipe p))
  -> Expr (f (QName Pipe))
  -> Expr (f (Either (QName Pipe) (SomePipe p)))
resolveNames lookupPipe = fmap (fmap tryLookupPipe)
  where
    tryLookupPipe :: QName Pipe -> Either (QName Pipe) (SomePipe p)
    tryLookupPipe name = maybeToEither name $ lookupPipe name

doCompile ::
  forall e p. (e ~ Text)
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

doAnalyse ::
  forall e p. e ~ Text
  => Expr (Located (Either (QName Pipe) (SomePipe p)))
  -> Either e (Expr (Located (CPipe p)))
doAnalyse = go emptyPipeCtx
 where
   emptyPipeCtx :: MSig
   emptyPipeCtx = Sig [] Nothing

   go :: MSig
      -> Expr (Located (Either (QName Pipe) (SomePipe p)))
      -> Either e (Expr (Located (CPipe p)))
   go (Sig args out) = \case
     PPipe (Locn l (Left pn)) -> Right . PPipe . Locn l $ CFreePipe (Sig (reverse args) out) pn
     PPipe (Locn l (Right p)) -> PPipe . Locn l <$> checkApply p (Sig (reverse args) out)

     PApp  PVal{} _           -> Left "Applying a value."
     PApp  f      PVal{vX}    ->
       go (Sig (Just (someValueSomeType vX) : args) out) f
     PVal{vX} -> Left $ "doCompile:  " <> pack (show vX)

   checkApply :: SomePipe p -> MSig -> Either e (CPipe p)
   checkApply p@(somePipeSig -> Sig params (I out)) args = case args of
     Sig []     Nothing    -> Right $ CSomePipe args p -- special case: unhinged..
     Sig eArgs  Nothing    -> maybeToLeft (CSomePipe args p) $
                                checkArgs eArgs params
     Sig eArgs (Just eRet) -> maybeToLeft (CSomePipe args p) $
                                checkTy "return value" p eRet out
                                  (checkArgs eArgs params)
    where
      checkArgs :: [Maybe SomeType] -> [I SomeType] -> Maybe e
      checkArgs ctx sig = goCheck 0 ctx sig
       where
         goCheck :: Int -> [Maybe SomeType] -> [I SomeType] -> Maybe e
         goCheck _ []    [] = Nothing -- success
         goCheck _ [] (_:_) = Just $ "Too many "   <> argcMiss (somePipeName p) argC paramC
         goCheck _ (_:_) [] = Just $ "Not enough " <> argcMiss (somePipeName p) argC paramC
         goCheck i (Nothing:ctx') (_:sig') =
           goCheck (i + 1) ctx' sig'
         goCheck i (Just argTy:ctx') (I paramTy:sig') =
           checkTy ("arg " <> showT i) p argTy paramTy
             (goCheck (i + 1) ctx' sig')

         paramC = length sig - 1
         argC   = length ctx - 1

         argcMiss :: Name Pipe -> Int -> Int -> e
         argcMiss name _paramC _argC = mconcat
           [ "args for pipe \"", showT name, "\":  "
           , showT paramC, " required, ", showT argC, " passed."]
      checkTy :: Text -> SomePipe p -> SomeType -> SomeType -> Maybe e -> Maybe e
      checkTy desc p act exp c
        | exp == act = c
        | otherwise = Just $ mconcat
          [ "Mismatch for ", desc, " "
          , "of pipe ", showT (somePipeName p), ": "
          , "expected: ", showT exp, ", "
          , "got: ", showT act
          ]

-- Represents a pipe, known or not.
data CPipe p
  = CFreePipe
    { cpArgs  :: !MSig
    , cfpName :: !(QName Pipe)
    }
  | CSomePipe
    { cpArgs  :: !MSig
    , cspPipe :: !(SomePipe p)
    }

instance Show (CPipe p) where
  show (CFreePipe args name) = mconcat
    [ show name
    , " :?: "
    , T.unpack $ T.intercalate " ⇨ "
      (maybe "(?)" (showSomeType False) <$> unListSig (toListSig args))]
  show (CSomePipe args G{..}) = unpack (showPipe gPipe)
  show (CSomePipe args T{..}) = unpack (showPipe tPipe)

showT :: Show a => a -> Text
showT = pack . show
