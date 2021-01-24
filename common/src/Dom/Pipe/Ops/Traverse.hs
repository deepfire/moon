{- OPTIONS_GHC -ddump-tc-trace -}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -fprint-explicit-foralls -fprint-explicit-kinds  -fprint-explicit-coercions #-}
module Dom.Pipe.Ops.Traverse (module Dom.Pipe.Ops.Traverse) where

import Algebra.Graph                    qualified as G
import Data.SOP                         qualified as SOP
import Data.Vector                      qualified as Vec
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
import Dom.SomeType
import Dom.Struct
import Dom.Tags

import Dom.Result
import Ground.Table() -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
demoTraverse :: IO ()
demoTraverse = case traverseP travDyn pipeFn pipeTr of
  Left e -> putStrLn $ show e
  Right p -> runSomePipe p & \case
    SR LNow ioa ->
      ioa >>= \case
        Left e -> putStrLn . unpack $ "runtime error: " <> showError e
        Right r -> putStrLn $ "traversed: " <> show r
 where
   pipeFn :: SomePipe Dynamic
   pipeFn = somePipe1 "demo pipe" LNow capsT CVPoint CVPoint
     (\x -> pure $ Right (x * 10 :: Integer))

   pipeTr :: SomePipe Dynamic
   pipeTr = somePipe0 "demo traversable" LNow capsT CVList
     (pure $ Right (Vec.fromList [1, 2, 3 :: Integer]))

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- traverseP ~:: Applicative f => (a -> f b) -> t a -> f (t b)
--
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
--
traverseP ::
     (forall l fas fa fav fo fov tas to tt
      . ( PipeConstr l fas fo
        , PipeConstr l tas to
        , tas ~ '[]
        , fas ~ (fa ': '[])
        , fa ~ CTagV Point fav
        , to ~ CTagV tt    fav
        , fo ~ CTagV Point fov
        )
      => Desc l fas fo -> p -> Desc l tas to -> p -> Fallible p)
  -> SomePipe p -> SomePipe p -> PFallible (SomePipe p)
traverseP pf spf spt =
  left ETrav $
  somePipeTraverse spf spt $
    \(f :: Pipe l fas fo p)
     (t :: Pipe l tas to p) ->
      if | Just HRefl <- typeRep @(CTagVC (Head fas)) `eqTypeRep` typeRep @Point
         , Just HRefl <- typeRep @(CTagVC fo)         `eqTypeRep` typeRep @Point
         , Just HRefl <- typeRep @(CTagVV to) `eqTypeRep`
                         typeRep @(CTagVV (Head fas))
         -> doTraverse pf f t
         | otherwise
         -> Left "Non-Point function or function/traversable type mismatch."

doTraverse ::
     forall l tas to tt a b fas fo ras ro p
   . ( PipeConstr l fas fo
     , PipeConstr l tas to
     , fas ~ (CTagV Point a ': '[])
     , tas ~ '[]
     , fo  ~ CTagV Point b
     , to  ~ CTagV tt    a
     , ras ~ '[]                   -- TODO:  undo this constraint
     , ro  ~ CTagV tt    b
     )
  => (Desc l fas fo -> p -> Desc l tas to -> p -> Fallible p)
  -> Pipe l     fas fo p
  -> Pipe l tas to     p
  -> Fallible (Pipe l ras ro p)
doTraverse pf
  P{ pDesc_=df@Desc{pdLTag=LNow}, pName=Name fn, pOutSty=fosty, pStruct=Struct fg
   , pArgs=_ SOP.:* Nil, pOut=Tags{tVTag=vtag}, pPipe=f}
  P{ pDesc_=dt@Desc{pdLTag=LNow}, pName=Name tn, pOutSty=tosty, pStruct=Struct tg
   , pArgs=Nil, pOut=Tags{tCTag=ctag}, pPipe=t}
  -- (Pipe df@(Desc (Name fn) _ (Struct fg) _  _ _  _ c) f)
  -- (Pipe dt@(Desc (Name fn) _ (Struct fg) _ ca a cb _) t)
  = Pipe desc <$> (pf df f dt t)
  where desc    = Desc name sig struct (SomeTypeRep rep) LNow ras ro
        ras     = Nil
        ro      = Tags ctag vtag
        name    = Name $ "("<>fn<>")-<trav>-("<>tn<>")"
        sig     = Sig [] (I $ someTypeFromConType tosty fosty)
        struct  = Struct (fg `G.overlay` tg) -- XXX: structure!
        rep     = typeRep :: TypeRep (IOA Now ras ro)
doTraverse _ _ _ = Left "Intraversible 3"
