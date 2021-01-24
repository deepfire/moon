{-# LANGUAGE PatternSynonyms #-}
module Dom.Pipe.Ops.Apply (module Dom.Pipe.Ops.Apply) where

import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis

import Data.Shelf

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Constr
import Dom.Pipe.IOA
import Dom.Pipe.Pipe
import Dom.Pipe.SomePipe
import Dom.Sig
import Dom.SomeValue
import Dom.Struct
import Dom.Value

import Dom.Result
import Ground.Table -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
demoApply :: IO ()
demoApply = case apply appDyn pipe val of
  Left e -> putStrLn $ show e
  Right p -> runSomePipe p & \case
    SR LNow ioa ->
      ioa >>= \case
        Left e -> putStrLn . unpack $ "runtime error: " <> showError e
        Right _r -> pure ()
 where
   pipe :: SomePipe Dynamic
   pipe = somePipe1 "demo pipe" LNow capsT CVPoint CVPoint
     ((>> pure (Right ())) . putStrLn . (<> " (c)(r)(tm)"))

   val :: SomeValue
   val = SV CPoint $ SVK VString capsT $ VPoint ("Apply!" :: String)

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- apply ~:: Pipe (a:as) o -> Value a -> Pipe as o
--
apply ::
     (forall l as as' o a
      . ( PipeConstr l as  o
        , PipeConstr l as' o
        , as ~ (a : as')
        )
      => Desc l as o -> Value (CTagVC a) (CTagVV a) -> p -> Fallible p)
  -> SomePipe p
  -> SomeValue
  -> PFallible (SomePipe p)
apply pf sp x = somePipeUncons sp
  (const "EApply: Cannot apply value to a saturated pipe.")
  $ \unsat ->
      apply' pf unsat x
      & mapFall (\(Error e) -> EApply . Error $ e
                  <> ".  Pipe rep: " <> showSomeTypeRep (somePipeRep sp)
                  <> ", Value rep: " <> showSomeTypeRep (someValueSomeTypeRep x)
                  <> ".")

apply' ::
    forall l a (as :: [*]) (as' :: [*]) o p
  . ( PipeConstr l as o
    , as ~ (a:as')
    )
  => (Desc l as o -> Value (CTagVC a) (CTagVV a) -> p -> Fallible p)
  -> Pipe l as o p
  -> SomeValue
  -> Fallible (Pipe l as' o p)
apply' pf
  f@P{pPipeRep=ioa@IOATyCons{tagARep=tA, aRep=a}}
  (SV _ (SVK _ vcaps (v :: Value cv v) :: SomeValueKinded cv)) =
  fromMaybe (error "impossible:  apply':  non-Typeable value leaked through the cracks!") $
  withOpenShelf vcaps CTypeable $
  if
    | Just e <- ioaTyConsInvalidity ioa
      -> fallDesc "Apply" e

    | Nothing <- typeRep @cv `eqTypeRep`  tA
      -> fallDesc "Apply: Value mismatch" $ show2 "cv" (typeRep @cv) "ca" tA
    | Nothing <- typeRep @v  `eqTypeRep`   a
      -> fallDesc "Apply: Con mismatch"   $ show2  "v" (typeRep @v)   "a"  a

    | Just HRefl <- typeRep @cv `eqTypeRep` tA
    , Just HRefl <- typeRep @cv `eqTypeRep` typeRep @(CTagVC a)
    , Just HRefl <- typeRep @v  `eqTypeRep`  a
    , Just HRefl <- typeRep @v  `eqTypeRep` typeRep @(CTagVV a)
      -> case spineConstraint of
        (Dict :: Dict Typeable as') -> doApply pf f v
    | otherwise
      -> Left "Apply: matched, but checks failed."
  where
    show2 :: Text -> TypeRep cvl -> Text -> TypeRep cvr -> Text
    show2 ln l rn r = ln<>"="<>pack (show l)<>", "<>rn<>"="<>pack (show r)
apply' _ P{pPipeRep=r} _ =
  fallDesc "Apply: typerep match fell through" $ pack (show r)
apply' _ _ _ =
  fall "Apply: typerep match fell through."

-- | 'doApply': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
doApply ::
     forall l as o c v p a ass
   . ( PipeConstr l as o
     , as ~ (a : ass))
  => (Desc l as o -> Value c v -> p -> Fallible p)
  -> Pipe l as o p
  -> Value c v
  -> Fallible (Pipe l (Tail as) o p)
doApply pf
        (Pipe desc@(Desc (Name rn) (Sig ras ro) (Struct rg) _ l (_a SOP.:* ass) o) f)
        v
  = case spineConstraint of
      (Dict :: Dict Typeable ass) ->
        Pipe (Desc (Name $ "("<>rn<>" val)")
                   (Sig (tail ras) ro)
                   (Struct rg)
                   (SomeTypeRep (typeRep :: TypeRep (IOA Now ass o)))
                   l
                   ass
                   o)
        <$> pf desc v f
