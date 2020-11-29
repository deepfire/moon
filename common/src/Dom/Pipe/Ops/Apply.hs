{-# LANGUAGE PatternSynonyms #-}
module Dom.Pipe.Ops.Apply (module Dom.Pipe.Ops.Apply) where

import           Data.Dynamic                       (fromDynamic)
import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis

import Dom.CTag
import Dom.Error
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Constr
import Dom.Pipe.IOA
import Dom.Pipe.SomePipe
import Dom.Sig
import Dom.SomeValue
import Dom.Struct
import Dom.Tags
import Dom.Value

-- import Ground.Table -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
-- demoApply :: IO ()
-- demoApply = case apply appDyn pipe val of
--   Left e -> putStrLn . unpack $ "apply error: " <> e
--   Right p -> runSomePipe p >>= \case
--     Left e -> putStrLn . unpack $ "runtime error: " <> e
--     Right _ -> pure ()
--  where
--    pipe :: SomePipe Dynamic
--    pipe = pipe1G "demo pipe" TPoint' TPoint'
--      ((>> pure (Right ())) . putStrLn . (<> " (c)(r)(tm)"))

--    val :: SomeValue
--    val = SomeValue TPoint $ SomeValueKinded VString $ VPoint ("demo!" :: String)

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- apply ~:: Pipe (ca:cas) o -> Value ca -> Pipe cas o
--
apply ::
     (forall g cas cas' o ca
      . ( PipeConstr g cas  o
        , PipeConstr g cas' o
        , cas ~ (ca : cas')
        )
      => Desc g cas o -> Value (TypesC ca) (TypesV ca) -> p -> Fallible p)
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
    forall g ca (cas :: [*]) (cas' :: [*]) o p
  . ( PipeConstr g cas o
    , cas ~ (ca:cas')
    )
  => (Desc g cas o -> Value (TypesC ca) (TypesV ca) -> p -> Fallible p)
  -> Pipe g cas o p
  -> SomeValue
  -> Fallible (Pipe g cas' o p)
apply' pf
  f@P{pPipeRep=ioa@IOATyCons{tagARep=tA, aRep=a}}
  (SomeValue _ (SomeValueKinded _ (v :: Value cv v) :: SomeValueKinded cv))
  | Just e <- ioaTyConsInvalidity ioa
  = fallDesc "Apply" e

  | Nothing <- typeRep @cv `eqTypeRep`  tA
  = fallDesc "Apply: Value mismatch" $ show2 "cv" (typeRep @cv) "ca" tA
  | Nothing <- typeRep @v  `eqTypeRep`   a
  = fallDesc "Apply: Con mismatch"   $ show2  "v" (typeRep @v)   "a"  a

  | Just HRefl <- typeRep @cv `eqTypeRep` tA
  , Just HRefl <- typeRep @cv `eqTypeRep` typeRep @(TypesC ca)
  , Just HRefl <- typeRep @v  `eqTypeRep`  a
  , Just HRefl <- typeRep @v  `eqTypeRep` typeRep @(TypesV ca)
  = case spineConstraint of
      (Dict :: Dict Typeable cas') -> doApply pf f v
  | otherwise
  = Left "Apply: matched, but checks failed."
  where
    show2 :: Text -> TypeRep l -> Text -> TypeRep r -> Text
    show2 ln l rn r = ln<>"="<>pack (show l)<>", "<>rn<>"="<>pack (show r)
apply' _ P{pPipeRep=r} _ =
  fallDesc "Apply: typerep match fell through" $ pack (show r)
apply' _ _ _ =
  fall "Apply: typerep match fell through."

-- | 'doApply': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
doApply ::
     forall g cas o c a p ca cass
   . ( PipeConstr g cas o
     , cas ~ (ca : cass))
  => (Desc g cas o -> Value c a -> p -> Fallible p)
  -> Pipe  g cas o p
  -> Value   c a
  -> Fallible (Pipe g (Tail cas) o p)
doApply pf
        (Pipe desc@(Desc (Name rn) (Sig ras ro) (Struct rg) _ (_ca SOP.:* cass) o) f)
        v
  = case spineConstraint of
      (Dict :: Dict Typeable cass) ->
        -- trace ("doApply: g=" <> (show $ typeRep @g)) $
        let desc'   = Desc name sig struct (SomeTypeRep rep) cass o
            name    = Name $ "app-"<>rn
            sig     = Sig (tail ras) ro
            struct  = Struct rg -- XXX ???
            rep     = typeRep :: TypeRep (IOA g cass o)
        in Pipe desc' <$> pf desc v f

appDyn ::
     forall g cas cass (o :: *) ca
   . ( PipeConstr g cas o
     , cas ~ (ca:cass)
     )
  => Desc g cas o -> Value (TypesC ca) (TypesV ca) -> Dynamic
  -> Fallible Dynamic
appDyn
  Desc {pdArgs = Tags _ _ SOP.:* _}
  v ioaDyn = case spineConstraint of
      (Dict :: Dict Typeable cass) ->
        Dynamic typeRep <$> case fromDynamic ioaDyn of
          Just (ioa :: IOA g cas o) -> Right $ applyIOA ioa v
          Nothing -> fallS $ printf
            "appDyn: invariant failure: as %s, o %s, dyn %s"
            (show $ typeRep @cas) (show $ typeRep @o) (show $ dynRep ioaDyn)

applyIOA ::
     forall g cas cass o c a
  .  ( PipeConstr g cas o
     , cas ~ (Types c a : cass)
     )
  => IOA g cas  o
  -> Value c a
  -> IOA g cass o
applyIOA
  (IOA (f :: PipeFunTy (Types c a:ass) o)
    c _as o
  ) v = case spineConstraint of
          (Dict :: Dict Typeable cas) ->
            IOA (applyPipeFun' f (Proxy @ass) o v :: PipeFunTy ass o)
             c (Proxy @ass) (Proxy @o)

-- | 'applyPipeFun': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
applyPipeFun' ::
     forall (cas :: [*]) (o :: *) (c :: Con) (a :: *)
  .  PipeFunTy (Types c a:cas) o
  -> Proxy cas
  -> Proxy o
  -> Value c a
  -> PipeFunTy cas o
applyPipeFun' f _ _ = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x

_applyPipeFun ::
     (Repr c a -> r) -> Value c a
  -> r
_applyPipeFun f = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x
