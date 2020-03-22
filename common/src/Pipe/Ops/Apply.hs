{-# LANGUAGE PatternSynonyms #-}
module Pipe.Ops.Apply
  ( apply
  , demo_apply
  )
where

import           Data.Dynamic                       (fromDynamic)
import qualified Data.Kind                        as K
import           Data.Maybe                         (fromJust)
import qualified Data.SOP                         as SOP
import qualified Data.SOP.Constraint              as SOP
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NP                  as SOP
import qualified Generics.SOP.NS                  as SOP
import           Type.Reflection

import Basis
import Pipe.Expr
import Pipe.Types
import Pipe.Zipper
import Pipe.Ops.Base
import Pipe.Ops.Internal
import Type

demo_apply :: IO ()
demo_apply = case apply appDyn pipe val of
  Left e -> putStrLn . unpack $ "apply error: " <> e
  Right p -> runPipe p >>= \case
    Left e -> putStrLn . unpack $ "runtime error: " <> e
    Right r -> pure ()
 where
   pipe :: SomePipe Dynamic
   pipe = linkG "demo pipe" TPoint' TPoint'
     ((>> pure (Right ())) . putStrLn . (<> " (c)(r)(tm)"))

   val :: SomeValue
   val = SomeValue $ SomeKindValue TPoint $ VPoint ("demo!" :: String)

apply
  :: (forall c kas kas' o ka
      . ( PipeConstr c kas  o
        , PipeConstr c kas' o
        , kas ~ (ka : kas')
        )
      => Desc c kas o -> Value (TagOf ka) (TypeOf ka) -> p -> Either Text p)
  -> SomePipe p
  -> SomeValue
  -> Either Text (SomePipe p)
apply pf sp x = somePipeUncons sp
  (const "Cannot apply value to a saturated pipe.")
  $ \unsat -> apply' pf unsat x
              & mapLeft (\e -> e
                          <> ".  Pipe rep: " <> showSomeTypeRep (somePipeRep sp)
                          <> ", Value rep: " <> showSomeTypeRep (someValueSomeTypeRep x)
                          <> ".")

apply'
  :: forall c ka (kas :: [*]) (kas' :: [*]) o p
  . ( PipeConstr c kas o
    , kas ~ (ka:kas')
    )
  => (Desc c kas o -> Value (TagOf ka) (TypeOf ka) -> p -> Either Text p)
  -> Pipe c kas o p
  -> SomeValue
  -> Either Text (Pipe c kas' o p)
apply' pf
  f@P{pPipeRep=ioa@IOATyCons{tagARep, aRep}}
  (SomeValue (SomeKindValue _ (v :: Value kv v) :: SomeKindValue v))
  | Just e <- ioaTyConsInvalidity ioa
  = Left $ "Apply: " <> e

  | Nothing <- typeRep @kv `eqTypeRep`  tagARep
  = Left $ "Apply: Value mismatch: " <> show2 "kv" (typeRep @kv) "ka" tagARep
  | Nothing <- typeRep @v  `eqTypeRep`  aRep
  = Left $ "Apply: Con mismatch: "   <> show2  "v" (typeRep @v)   "a"  aRep

  | Just HRefl <- typeRep @kv `eqTypeRep` tagARep
  , Just HRefl <- typeRep @kv `eqTypeRep` typeRep @(TagOf ka)
  , Just HRefl <- typeRep @v  `eqTypeRep`  aRep
  , Just HRefl <- typeRep @v  `eqTypeRep` typeRep @(TypeOf ka)
  = case spineConstraint of
      (Dict :: Dict Typeable kas') -> doApply pf f v
  | otherwise
  = Left "Apply: matched, but checks failed."
  where
    show2 :: Text -> TypeRep l -> Text -> TypeRep r -> Text
    show2 ln l rn r = ln<>"="<>pack (show l)<>", "<>rn<>"="<>pack (show r)

apply' _ P{pPipeRep} _ =
  Left $ "Apply: typerep match fell through: " <> pack (show pPipeRep)

-- | 'doApply': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
doApply
  :: forall c kas o k a ra rb p ka kass
   . ( PipeConstr c kas o
     , kas ~ (ka : kass))
  => (Desc c kas o -> Value k a -> p -> Either Text p)
  -> Pipe  c kas o p
  -> Value   k a
  -> Either Text (Pipe c (Tail kas) o p)
doApply pf
        (Pipe desc@(Desc (Name rn) (Sig ras ro) (Struct rg) _ (ka SOP.:* kass) o) f)
        v
  = case spineConstraint of
      (Dict :: Dict Typeable kass) ->
        let desc'   = Desc name sig struct (SomeTypeRep rep) kass o
            name    = Name $ "app-"<>rn
            sig     = Sig (tail ras) ro
            struct  = Struct rg -- XXX ???
            rep     = typeRep :: TypeRep (IOA c kass o)
        in Pipe desc' <$> pf desc v f

appDyn
  :: forall c kas kass (o :: *) f ka f'
   . ( PipeConstr c kas o
     , kas ~ (ka:kass)
     )
  => Desc c kas o -> Value (TagOf ka) (TypeOf ka) -> Dynamic
  -> Either Text Dynamic
appDyn
  Desc {pdArgs = (TypePair _ _) SOP.:* _}
  v ioaDyn = case spineConstraint of
      (Dict :: Dict Typeable kass) ->
        Dynamic typeRep <$> case fromDynamic ioaDyn of
          Just (ioa :: IOA c kas o) -> Right $ applyIOA ioa v
          Nothing -> Left . pack $ printf
            "appDyn: invariant failure: as %s, o %s, dyn %s"
            (show $ typeRep @kas) (show $ typeRep @o) (show $ dynRep ioaDyn)

applyIOA
  :: forall c kas kass o k a
  .  ( PipeConstr c kas o
     , kas ~ (Type k a : kass)
     )
  => IOA c kas  o
  -> Value k a
  -> IOA c kass o
applyIOA
  (IOA (f :: PipeFunTy (Type k a:ass) o)
    c as o
  ) v = case spineConstraint of
  (Dict :: Dict Typeable kas) ->
    (IOA (applyPipeFun' f (Proxy @ass) o v :: PipeFunTy ass o)
     c (Proxy @ass) (Proxy @o))

-- | 'applyPipeFun': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
applyPipeFun'
  :: forall (kas :: [*]) (o :: *) (k :: Con) (a :: *)
  .  PipeFunTy (Type k a:kas) o
  -> Proxy kas
  -> Proxy o
  -> Value k a
  -> PipeFunTy kas o
applyPipeFun' f _ _ = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x

applyPipeFun
  :: (Repr k a -> r) -> Value k a
  -> r
applyPipeFun f = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x
