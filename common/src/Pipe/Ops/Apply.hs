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
  :: (forall c kas kas' o k a
      . ( PipeConstr c kas  o
        , PipeConstr c kas' o
        , kas ~ (Type k a : kas'))
      => Desc c kas o -> Value k a -> p -> Either Text p)
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
  :: forall c (k :: Con) (a :: *) (kas :: [*]) (kas' :: [*]) o p
  . ( PipeConstr c kas o
    , kas ~ (Type k a:kas')
    )
  => (Desc c kas o -> Value k a -> p -> Either Text p)
  -> Pipe c kas o p
  -> SomeValue
  -> Either Text (Pipe c kas' o p)
apply' pf
  f@(P _ _ ioa@(IOATyCons _ _ _ ka a _ _ _) _ _ _)
    (SomeValue (SomeKindValue _ (v :: Value kv v) :: SomeKindValue v))
  | Just e <- ioaTyConsInvalidity ioa
  = Left $ "Apply: " <> e

  | Nothing <- typeRep @kv `eqTypeRep`  ka
  = Left $ "Apply: Value mismatch: " <> show2 "kv" (typeRep @kv) "ka" ka
  | Nothing <- typeRep @v  `eqTypeRep`  a
  = Left $ "Apply: Con mismatch: "   <> show2  "v" (typeRep @v)   "a"  a

  | Just HRefl <- typeRep @kv `eqTypeRep` ka
  , Just HRefl <- typeRep @kv `eqTypeRep` typeRep @k
  , Just HRefl <- typeRep @v  `eqTypeRep`  a
  , Just HRefl <- typeRep @v  `eqTypeRep` typeRep @a
  = case spineConstraint of
      (Dict :: Dict Typeable kas') -> doApply pf f v
  | otherwise
  = Left "Apply: fall through."
  where
    show2 :: Text -> TypeRep l -> Text -> TypeRep r -> Text
    show2 ln l rn r = ln<>"="<>pack (show l)<>", "<>rn<>"="<>pack (show r)

apply' _ (P _ _ tr _ _ _) _ =
  Left $ "Typerep structure mismatch: " <> pack (show tr)

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
  :: forall c kas kass (o :: *) f k a f'
   . ( PipeConstr c kas o
     , kas ~ (Type k a:kass)
     )
  => Desc c kas o -> Value k a -> Dynamic
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
