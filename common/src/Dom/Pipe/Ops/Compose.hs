{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Dom.Pipe.Ops.Compose (module Dom.Pipe.Ops.Compose) where

import qualified Algebra.Graph                    as G
import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis

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
import Dom.Struct

import Dom.Result
import Ground.Table() -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
demoCompose :: IO ()
demoCompose = case compose compDyn pipe val of
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

   val :: SomePipe Dynamic
   val = somePipe0 "demo value" LNow capsT CVPoint
     (pure $ Right ("compose!" :: String))

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- compose ~:: (b -> c) -> (a -> b) -> a -> c
--
compose ::
     (forall l vas vo fas fass fo
      . ( PipeConstr l vas vo
        , PipeConstr l fas fo
        , fas ~ (vo:fass)
        )
      => (Desc l vas vo -> p -> Desc l fas fo -> p -> Fallible p))
  -> SomePipe p
  -> SomePipe p
  -> PFallible (SomePipe p)
compose pf f v =
  somePipeUncons f
  (const $ EComp "Cannot compose value and a saturated pipe.") $
  \(f' :: Pipe l1 (a : as) fo p) ->
    withSomePipe v $
    \(v' :: Pipe l2 _vas vo p) ->
      case (typeRep @a  `eqTypeRep` typeRep @vo,
            typeRep @l1 `eqTypeRep` typeRep @l2) of
        (Nothing, _) -> Left $ EComp "Compose: v out != f arg"
        (Just HRefl, Just HRefl) -> left EComp $ compose'' pf f' v'

compose'' ::
    forall l vas vo fas fass ras fo p
  . ( PipeConstr l vas vo
    , PipeConstr l fas fo
    , fas ~ (vo:fass)
    , ras ~ fass
    )
  => (forall vas' vo' fas' fass' ras' fo'
       . ( PipeConstr l vas' vo'
         , PipeConstr l fas' fo'
         , fas' ~ (vo':fass')
         , ras' ~ fass'
         )
      => Desc l vas' vo' -> p -> Desc l fas' fo' -> p -> Fallible p)
  -> Pipe l fas fo p
  -> Pipe l vas vo p
  -> Fallible (Pipe l ras fo p)
compose'' pf
  -- | Just HRefl <- typeRep @ct2 `eqTypeRep` typeRep @cf1
  -- , Just HRefl <- typeRep @tt2 `eqTypeRep` typeRep @tf1
  | True
  = compose' pf
  | otherwise
  = \l r -> fall $ "'compose' failed on: l="<>pack (show l)<>" r="<>pack (show r)

-- | 'compose': approximate '(.)'
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ..and convert to
-- (=<<) :: (a -> m b) -> m a -> m b
compose' ::
     forall l vas vo fas fass ras fo p
   . ( PipeConstr l vas vo
     , PipeConstr l fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => (Desc l vas vo -> p -> Desc l fas fo -> p -> Fallible p)
  -> Pipe l fas fo p
  -> Pipe l vas vo p
  -> Fallible (Pipe l ras fo p)
compose' composeF
  pF@P{pPipeRep=frep}
  pV@P{pPipeRep=vrep}
  | Just e <- ioaTyConsInvalidity frep
  = fall $ "Compose: sink function: " <> e
  | Just e <- ioaTyInvalidity vrep
  = fall $ "Compose: source function: " <> e

  | otherwise
  = doBind composeF pF pV
compose' _ f v
  = fall $ "compose: incompatible: f="<>pack (show f)<>" v="<>pack (show v)

-- | 'doBind': approximate 'bind':
-- (=<<) :: (a -> m b) -> m a -> m b
-- ..with the difference that we should handle a FLink on the left as well,
-- ..but not just yet.
doBind ::
     forall l vr fr fas fass ras fo vas vo p
   . ( PipeConstr l vas vo, vr ~ ReprOf vo
     , PipeConstr l fas fo, fr ~ ReprOf fo
     -- this is hard-coded to a 0-ary function being applied to a 1-ary one
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => (Desc l vas vo     -> p ->
      Desc l    fas fo -> p ->
      Fallible p)
  -> Pipe l        fas fo p
  -> Pipe l vas vo        p
  -> Fallible (Pipe l fass fo p)
doBind pf
  P{ pDesc_=df@Desc{pdLTag=LNow}, pName=Name fn, pArgStys=sfas, pOutSty=sfo, pStruct=Struct fg
   , pArgs=(_fa SOP.:* fass), pOut=fo, pPipe=f}
  P{ pDesc_=dv@Desc{pdLTag=LNow}, pName=Name vn, pStruct=Struct vg
   , pOut=_vo, pPipe=v}
  = Pipe desc <$> pf dv v df f
 where
   desc    = Desc name sig struct (SomeTypeRep rep) LNow fass fo
   name    = Name $ "("<>fn<>") >>= ("<>vn<>")"
   sig     = Sig (I <$> tail sfas) (I sfo)
   struct  = Struct $ G.overlay fg vg
   rep     = case spineConstraint of
               (Dict :: Dict Typeable fass) ->
                 typeRep :: TypeRep (IOA Now fass fo)
doBind _ _ _ = error "doBind"
