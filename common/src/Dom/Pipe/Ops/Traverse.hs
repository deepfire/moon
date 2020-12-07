{- OPTIONS_GHC -ddump-tc-trace -}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -fprint-explicit-foralls -fprint-explicit-kinds  -fprint-explicit-coercions #-}
module Dom.Pipe.Ops.Traverse (module Dom.Pipe.Ops.Traverse) where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
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
import Dom.VTag

import Ground.Table() -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
demoTraverse :: IO ()
demoTraverse = case traverseP travDyn pipeFn pipeTr of
  Left e -> putStrLn $ show e
  Right p -> runSomePipe p >>= \case
    Left e -> putStrLn . unpack $ "runtime error: " <> showError e
    Right r -> putStrLn $ "traversed: " <> show r
 where
   pipeFn :: SomePipe Dynamic
   pipeFn = somePipe1 "demo pipe" capsT CVPoint CVPoint
     (\x -> pure $ Right (x * 10 :: Integer))

   pipeTr :: SomePipe Dynamic
   pipeTr = somePipe0 "demo traversable" capsT CVList
     (pure $ Right ([1, 2, 3] :: [Integer]))

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- traverseP ~:: Applicative f => (a -> f b) -> t a -> f (t b)
--
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
--
traverseP ::
     (forall fas fa fav fo fov tas to tt
      . ( PipeConstr fas fo
        , PipeConstr tas to
        , tas ~ '[]
        , fas ~ (fa ': '[])
        , fa ~ CTagV Point fav
        , to ~ CTagV tt    fav
        , fo ~ CTagV Point fov
        )
      => Desc fas fo -> p -> Desc tas to -> p -> Fallible p)
  -> SomePipe p -> SomePipe p -> PFallible (SomePipe p)
traverseP pf spf spt =
  left ETrav $
  somePipeTraverse spf spt $
    \(f :: Pipe fas fo p)
     (t :: Pipe tas to p) ->
      if | Just HRefl <- typeRep @(CTagVC (Head fas)) `eqTypeRep` typeRep @Point
         , Just HRefl <- typeRep @(CTagVC fo)         `eqTypeRep` typeRep @Point
         , Just HRefl <- typeRep @(CTagVV to) `eqTypeRep`
                         typeRep @(CTagVV (Head fas))
         -> doTraverse pf f t
         | otherwise
         -> Left "Non-Point function or function/traversable type mismatch."

doTraverse ::
     forall tas to tt a b fas fo ras ro p
   . ( PipeConstr fas fo
     , PipeConstr tas to
     , fas ~ (CTagV Point a ': '[])
     , tas ~ '[]
     , fo  ~ CTagV Point b
     , to  ~ CTagV tt    a
     , ras ~ '[]                   -- TODO:  undo this constraint
     , ro  ~ CTagV tt    b
     )
  => (Desc fas fo -> p -> Desc tas to -> p -> Fallible p)
  -> Pipe     fas fo p
  -> Pipe tas to     p
  -> Fallible (Pipe ras ro p)
doTraverse pf
  P{ pDesc_=df, pName=Name fn, pOutSty=fosty, pStruct=Struct fg
   , pArgs=_ SOP.:* Nil, pOut=Tags{tVTag=vtag}, pPipe=f}
  P{ pDesc_=dt, pName=Name tn, pOutSty=tosty, pStruct=Struct tg
   , pArgs=Nil, pOut=Tags{tCTag=ctag}, pPipe=t}
  -- (Pipe df@(Desc (Name fn) _ (Struct fg) _  _ _  _ c) f)
  -- (Pipe dt@(Desc (Name fn) _ (Struct fg) _ ca a cb _) t)
  = Pipe desc <$> (pf df f dt t)
  where desc    = Desc name sig struct (SomeTypeRep rep) ras ro
        ras     = Nil
        ro      = Tags ctag vtag
        name    = Name $ "("<>fn<>")-<trav>-("<>tn<>")"
        sig     = Sig [] (I $ someTypeFromConType tosty fosty)
        struct  = Struct (fg `G.overlay` tg) -- XXX: structure!
        rep     = typeRep :: TypeRep (IOA ras ro)
doTraverse _ _ _ = Left "Intraversible 3"
-- XXX: where is the best place for this check now?
-- doTraverse l _ _ r _ _
--   = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

travDyn ::
     forall tas to tt a b fas fo ro
   . ( PipeConstr fas fo
     , PipeConstr tas to
     , fas ~ (CTagV Point a ': '[])
     , tas ~ '[]
     , fo  ~ CTagV Point b
     , to  ~ CTagV tt    a
     , ro  ~ CTagV tt    b
     )
  => Desc     fas fo -> Dynamic
  -> Desc tas to     -> Dynamic
  -> Fallible Dynamic
travDyn _df f dt t = Dynamic typeRep <$>
  case (fromDynamic f, fromDynamic t) of
    ( Just (IOA f' _   _fo :: IOA     fas fo)
     ,Just (IOA t' tas _to :: IOA tas to))
      -> Right (IOA ioa tas (Proxy @ro) :: IOA tas ro)
     where
       ioa :: Result (ReprOf ro)
       ioa = traversePipes0 (descOutCTag dt) (descOutVTag dt) (Proxy @b) f' t'
    (_, Nothing) -> fallS $ printf
      "travDyn: invariant failure: tas %s, to %s, dyn %s"
      (show $ typeRep @tas) (show $ typeRep @to) (show $ dynRep f)
    (Nothing, _) -> fallS $ printf
      "travDyn: invariant failure: fas %s, fo %s, dyn %s"
      (show $ typeRep @fas) (show $ typeRep @fo) (show $ dynRep t)

-- We have a conceptualisation problem.
--
-- On the one hand,
-- 1. traversePipes must accept a lambda
-- 2. whose ONLY argument will be passed
-- 3. as the ONLY acceptable argument to the traversable's generator
--
-- Full stop, end of story.  The return type:
--   (Repr tta ta -> Result (Repr ttr fr))
-- ..cannot represent non-unary functions.
-- We need a more powerful Repr, before we can ascend to higher arities.
--
-- As an additional side thought -- we'd probably have to abandon the
-- current args/out distinction, since it introduces non-uniformity?
-- Or is the tradeoff acceptable?
--
-- TODO: ^^^

-- | 'traversePipes': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- a ~= arg, r ~= res
traversePipes0 ::
    forall ttr tr fr
  . ( Typeable tr, Typeable fr
    )
  -- TODO:  see if we can just pass a Tags here
  => CTag ttr -> VTag tr -> Proxy fr
  -> (Repr Point tr -> Result (Repr Point fr))
  -> Result (Repr ttr tr)
  -> Result (Repr ttr fr)
traversePipes0 ttr _ _ f t = do
  tv <- t
  case tv :: Fallible (Repr ttr tr) of
    Left e -> pure $ Left e
    Right (x :: Repr ttr tr) ->
      case ttr of
        CPoint -> f x
        CList  -> sequence <$> traverse f x
        CSet   -> sequence <$> traverse f x
        CTree  -> fallM "traverse: Tree unsupported"
        CDag   -> fallM "traverse: Dag unsupported"
        CGraph -> fallM "traverse: Graph unsupported"

traversePipes1 ::
    forall tta ta ttr tr fr
  . ( Typeable ta, Typeable tr, Typeable fr
    )
  => CTag tta -> Proxy ta -> CTag ttr -> Proxy tr -> Proxy fr
  -> (tr -> Result fr)
  -> (Repr tta ta -> Result (Repr ttr tr))
  -> (Repr tta ta -> Result (Repr ttr fr))
traversePipes1 _ _ cb _ _ f t = \ra -> do
  tv <- t ra
  case tv :: Fallible (Repr ttr tr) of
    Left e -> pure $ Left e
    Right (x :: Repr ttr tr) ->
      case cb of
        CPoint -> f x
        CList  -> sequence <$> traverse f x
        CSet   -> sequence <$> traverse f x
        CTree  -> fallM "traverse: Tree unsupported"
        CDag   -> fallM "traverse: Dag unsupported"
        CGraph -> fallM "traverse: Graph unsupported"
