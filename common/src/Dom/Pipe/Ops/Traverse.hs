module Dom.Pipe.Ops.Traverse (module Dom.Pipe.Ops.Traverse) where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis

import Dom.CTag
import Dom.Error
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Pipe.IOA
import Dom.Pipe.SomePipe
import Dom.Sig
import Dom.SomeType
import Dom.Struct
import Dom.Tags
import Dom.VTag

-- import Ground.Table -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
demoTraverse :: IO ()
demoTraverse = undefined

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- traverseP ~:: Applicative f => (a -> f b) -> t a -> f (t b)
--
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
--
traverseP ::
     (forall cf ct fas fo tas to
      . ( PipeConstr cf fas fo
        , PipeConstr ct tas to
        , fas ~ (Types Point a ': '[])
        , tas ~ '[]
        , fo  ~ Types Point b
        , to  ~ Types tt    a
        , ro  ~ Types tt    b
        )
      => Desc cf fas fo -> p -> Desc ct tas to -> p -> Fallible p)
  -> SomePipe p -> SomePipe p -> Fallible (SomePipe p)
traverseP _ _ = undefined
  -- case f of
  --   G f' ->
  --     pipeArityCase f'
  --       (const $ Left "Cannot traverse with a saturated pipe.")
  --       (const $ Left "Cannot traverse with a pipe of arity above one.") $
  --       \(f'' :: Pipe cf fa fo p) ->
  --         case t of
  --           -- traverseP'' => o == ca
  --           -- forall ca (cas' :: [*]) c3.
  --           -- ((cas :: [*]) ~ ((':) @* ca cas' :: [*]), PipeConstr Ground cas' o,
  --           --  (cas' :: [*]) ~ ('[] @* :: [*])) =>
  --           -- Pipe Ground cas o p -> Fallible (SomePipe p)
  --           G t' -> G <$> traverseP'' Proxy pf f' t'
  --           T t' -> G <$> traverseP'' Proxy pf f' t'
    -- T f' ->
  -- withSomePipe f $
  -- \(f' :: Pipe cf fa fo p) ->
      -- pipeArityCase f'
      --   (const $ Left "Cannot traverse with a saturated pipe.")
      --   (const $ Left "Cannot traverse with a pipe of arity above one.") $
      --   \(f'' :: Pipe cf fa fo p) ->
      --     case t of
      --       G t' -> T <$> traverseP'' Proxy pf f' t'
      --       T t' -> T <$> traverseP'' Proxy pf f' t'
      -- withSomePipe t $
      -- \(t' :: Pipe ct _tas to p) ->
      --         traverseP'' Proxy pf f' t'

traverseP'' ::
     forall cf ct fas fo ft fa tas ttr tr to ras ro p proxy
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , PipeConstr cf ras ro
     , fas ~ (fa:'[])
     , fo ~ Types ft fa
     , to ~ Types ttr tr
     )
  => proxy (Types (TypesC to) (TypesV fo))
  -> (forall cf' ct' fas' fo' tas' to'
      . ( PipeConstr cf' fas' fo'
        , PipeConstr ct' tas' to')
      => Desc cf' fas' fo' -> p -> Desc ct' tas' to' -> p -> Fallible p)
  -> Pipe cf     fas fo p
  -> Pipe ct tas to     p
  -> Fallible (Pipe cf ras ro p)
traverseP'' _p pf f@P{pPipeRep=fioa} t@P{pPipeRep=tioa}
  | Just e <- ioaTySingletonInvalidity fioa = fall $ "Traverse: funty: " <> e
  | Just e <- ioaTyNilInvalidity tioa = fall $ "Traverse: traversablety: " <> e
  | Just HRefl <- typeRep @tr          `eqTypeRep` typeRep @(TypesV fa)
  , Just HRefl <- typeRep @(TypesC ro)  `eqTypeRep` typeRep @ttr
  , Just HRefl <- typeRep @(TypesV ro) `eqTypeRep` typeRep @(TypesV fo)
  , Just HRefl <- typeRep @(TypesC fa)  `eqTypeRep` typeRep @Point
  , Just HRefl <- typeRep @ft          `eqTypeRep` typeRep @Point
  , Just HRefl <- case spineConstraint of
                    (Dict :: Dict Typeable tas) -> typeRepNull $ typeRep @tas
  , Just HRefl <- case spineConstraint of
                    (Dict :: Dict Typeable ras) -> typeRepNull $ typeRep @ras
  = traverseP' pf f t
  | otherwise
  = fall $ "traverseP:  fell through on: f="<>pack (show f)<>" t="<>pack (show t)
 where
   showLR :: Text -> Text -> Text
   showLR l r = "left "<>l<>", right "<>r

   showLRP :: Pipe c1 as1 o1 Dynamic -> Pipe c2 as2 o2 Dynamic -> Text
   showLRP l r = showLR (showPipe l) (showPipe r)

traverseP'' _ _ _ _ = Left "traverseP'':  fell through absurdly bad."

traverseP' ::
     forall cf ct tas to tt a b fas fo ras ro p
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , tas ~ '[],                    to  ~ Types tt    a
     , fas ~ (Types Point a ': '[]), fo  ~ Types Point b
     , ras ~ '[],                    ro  ~ Types tt    b
     )
  => (Desc cf fas fo -> p ->
      Desc ct tas to -> p ->
      Fallible p)
  -> Pipe cf     fas fo p
  -> Pipe ct tas to     p
  -> Fallible (Pipe cf ras ro p)
traverseP' pf f@P{} t@P{}
  -- | Just e <- ioaTySingletonInvalidity fioa = Left $ "Traverse: funty: " <> e
  -- | Just e <- ioaTyNilInvalidity tioa = Left $ "Traverse: traversablety: " <> e
  | otherwise = doTraverse pf f t

doTraverse ::
     forall cf ct tas to tt a b fas fo ras ro p
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , fas ~ (Types Point a ': '[])
     , tas ~ '[]
     , fo  ~ Types Point b
     , to  ~ Types tt    a
     , ras ~ '[]                   -- TODO:  undo this constraint
     , ro  ~ Types tt    b
     )
  => (Desc cf fas fo -> p -> Desc ct tas to -> p -> Fallible p)
  -> Pipe cf     fas fo p
  -> Pipe ct tas to     p
  -> Fallible (Pipe cf ras ro p)
doTraverse pf
  P{ pDesc_=df, pName=Name fn, pOutSty=fosty, pStruct=Struct fg
   , pArgs=(_fca SOP.:* Nil), pOut=Tags{tVTag=vtag}, pPipe=f}
  P{ pDesc_=dt, pName=Name tn, pOutSty=tosty, pStruct=Struct tg
   , pArgs=(_tca SOP.:* Nil), pOut=Tags{tCTag=ctag}, pPipe=t}
  -- (Pipe df@(Desc (Name fn) _ (Struct fg) _  _ _  _ c) f)
  -- (Pipe dt@(Desc (Name fn) _ (Struct fg) _ ca a cb _) t)
  = Pipe desc <$> (pf df f dt t)
  where desc    = Desc name sig struct (SomeTypeRep rep) ras ro
        ras     = Nil
        ro      = Tags ctag vtag
        name    = Name $ "("<>fn<>")-<trav>-("<>tn<>")"
        sig     = Sig [] (I $ someTypeFromConType tosty fosty)
        struct  = Struct (fg `G.overlay` tg) -- XXX: structure!
        rep     = typeRep :: TypeRep (IOA cf ras ro)
-- XXX: where is the best place for this check now?
-- doTraverse l _ _ r _ _
--   = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

travDyn ::
     forall cf ct tas to tt a b fas fo ro
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , fas ~ (Types Point a ': '[])
     , tas ~ '[]
     , fo  ~ Types Point b
     , to  ~ Types tt    a
     , ro  ~ Types tt    b
     )
  => Desc cf     fas fo -> Dynamic
  -> Desc ct tas to     -> Dynamic
  -> Fallible Dynamic
travDyn _df f dt t = Dynamic typeRep <$>
  case (fromDynamic f, fromDynamic t) of
    ( Just (IOA f' cf _   _fo :: IOA cf     fas fo)
     ,Just (IOA t' _  tas _to :: IOA ct tas to))
      -> Right (IOA ioa cf tas (Proxy @ro) :: IOA cf tas ro)
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
        TPoint -> fallM "traverse: asked to traverse a Point"
        TList  -> sequence <$> traverse f x
        TSet   -> fallM "traverse: Set unsupported"
        TTree  -> fallM "traverse: Tree unsupported"
        TDag   -> fallM "traverse: Dag unsupported"
        TGraph -> fallM "traverse: Graph unsupported"

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
        TPoint -> f x
        TList  -> sequence <$> traverse f x
        TSet   -> fallM "traverse: Set unsupported"
        TTree  -> fallM "traverse: Tree unsupported"
        TDag   -> fallM "traverse: Dag unsupported"
        TGraph -> fallM "traverse: Graph unsupported"
