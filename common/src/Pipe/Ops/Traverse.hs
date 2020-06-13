module Pipe.Ops.Traverse
  ( traverseP
  , demoTraverse
  , travDyn
  , traverseP''
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis
import Pipe.Types
import Pipe.Ops.Internal
import Type


demoTraverse :: IO ()
demoTraverse = undefined

traverseP
  :: (forall cf ct fas fo tas to
      . ( PipeConstr cf fas fo
        , PipeConstr ct tas to
        , fas ~ (Type Point a ': '[])
        , tas ~ '[]
        , fo  ~ Type Point b
        , to  ~ Type tt    a
        , ro  ~ Type tt    b
        )
      => Desc cf fas fo -> p -> Desc ct tas to -> p -> Either Text p)
  -> SomePipe p -> SomePipe p -> Either Text (SomePipe p)
traverseP _ _ = undefined
  -- case f of
  --   G f' ->
  --     pipeArityCase f'
  --       (const $ Left "Cannot traverse with a saturated pipe.")
  --       (const $ Left "Cannot traverse with a pipe of arity above one.") $
  --       \(f'' :: Pipe cf fa fo p) ->
  --         case t of
  --           -- traverseP'' => o == ka
  --           -- forall ka (kas' :: [*]) k3.
  --           -- ((kas :: [*]) ~ ((':) @* ka kas' :: [*]), PipeConstr Ground kas' o,
  --           --  (kas' :: [*]) ~ ('[] @* :: [*])) =>
  --           -- Pipe Ground kas o p -> Either Text (SomePipe p)
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

traverseP''
  :: forall cf ct fas fo ft fa tas ttr tr to ras ro p proxy
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , PipeConstr cf ras ro
     , fas ~ (fa:'[])
     , fo ~ Type ft fa
     , to ~ Type ttr tr
     )
  => proxy (Type (TagOf to) (TypeOf fo))
  -> (forall cf' ct' fas' fo' tas' to'
      . ( PipeConstr cf' fas' fo'
        , PipeConstr ct' tas' to')
      => Desc cf' fas' fo' -> p -> Desc ct' tas' to' -> p -> Either Text p)
  -> Pipe cf     fas fo p
  -> Pipe ct tas to     p
  -> Either Text (Pipe cf ras ro p)
traverseP'' _p pf f@P{pPipeRep=fioa} t@P{pPipeRep=tioa}
  | Just e <- ioaTySingletonInvalidity fioa = Left $ "Traverse: funty: " <> e
  | Just e <- ioaTyNilInvalidity tioa = Left $ "Traverse: traversablety: " <> e
  | Just HRefl <- typeRep @tr          `eqTypeRep` typeRep @(TypeOf fa)
  , Just HRefl <- typeRep @(TagOf ro)  `eqTypeRep` typeRep @ttr
  , Just HRefl <- typeRep @(TypeOf ro) `eqTypeRep` typeRep @(TypeOf fo)
  , Just HRefl <- typeRep @(TagOf fa)  `eqTypeRep` typeRep @Point
  , Just HRefl <- typeRep @ft          `eqTypeRep` typeRep @Point
  , Just HRefl <- case spineConstraint of
                    (Dict :: Dict Typeable tas) -> typeRepNull $ typeRep @tas
  , Just HRefl <- case spineConstraint of
                    (Dict :: Dict Typeable ras) -> typeRepNull $ typeRep @ras
  = traverseP' pf f t
  | otherwise
  = Left $ "traverseP:  fell through on: f="<>pack (show f)<>" t="<>pack (show t)
traverseP'' _ _ _ _ = Left "traverseP'':  fell through absurdly bad."

traverseP'
  :: forall cf ct tas to tt a b fas fo ras ro p
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , tas ~ '[],                   to  ~ Type tt    a
     , fas ~ (Type Point a ': '[]), fo  ~ Type Point b
     , ras ~ '[],                   ro  ~ Type tt    b
     )
  => (Desc cf fas fo -> p ->
      Desc ct tas to -> p ->
      Either Text p)
  -> Pipe cf     fas fo p
  -> Pipe ct tas to     p
  -> Either Text (Pipe cf ras ro p)
traverseP' pf f@P{} t@P{}
  -- | Just e <- ioaTySingletonInvalidity fioa = Left $ "Traverse: funty: " <> e
  -- | Just e <- ioaTyNilInvalidity tioa = Left $ "Traverse: traversablety: " <> e
  | otherwise = doTraverse pf f t

-- | 'doTraverse': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
doTraverse
  :: forall cf ct tas to tt a b fas fo ras ro p
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , fas ~ (Type Point a ': '[])
     , tas ~ '[]
     , fo  ~ Type Point b
     , to  ~ Type tt    a
     , ras ~ '[]                   -- TODO:  undo this constraint
     , ro  ~ Type tt    b
     )
  => (Desc cf fas fo -> p -> Desc ct tas to -> p -> Either Text p)
  -> Pipe cf     fas fo p
  -> Pipe ct tas to     p
  -> Either Text (Pipe cf ras ro p)
doTraverse pf
  P{ pDesc_=df, pName=Name fn, pOutSty=fosty, pStruct=Struct fg
   , pArgs=(_fka SOP.:* Nil), pOut=TypePair{tpType=fty}, pPipe=f}
  P{ pDesc_=dt, pName=Name tn, pOutSty=tosty, pStruct=Struct tg
   , pArgs=(_tka SOP.:* Nil), pOut=TypePair{tpTag=ttag}, pPipe=t}
  -- (Pipe df@(Desc (Name fn) _ (Struct fg) _  _ _  _ c) f)
  -- (Pipe dt@(Desc (Name fn) _ (Struct fg) _ ka a kb _) t)
  = Pipe desc <$> (pf df f dt t)
  where desc    = Desc name sig struct (SomeTypeRep rep) ras ro
        ras     = Nil
        ro      = TypePair ttag fty
        name    = Name $ "("<>fn<>")-<trav>-("<>tn<>")"
        sig     = Sig [] (someTypeFromConType tosty fosty)
        struct  = Struct (fg `G.overlay` tg) -- XXX: structure!
        rep     = typeRep :: TypeRep (IOA cf ras ro)
-- XXX: where is the best place for this check now?
-- doTraverse l _ _ r _ _
--   = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

travDyn
  :: forall cf ct tas to tt a b fas fo ro
   . ( PipeConstr cf fas fo
     , PipeConstr ct tas to
     , fas ~ (Type Point a ': '[])
     , tas ~ '[]
     , fo  ~ Type Point b
     , to  ~ Type tt    a
     , ro  ~ Type tt    b
     )
  => Desc cf     fas fo -> Dynamic
  -> Desc ct tas to     -> Dynamic
  -> Either Text Dynamic
travDyn _df f dt t = Dynamic typeRep <$>
  case (fromDynamic f, fromDynamic t) of
    ( Just (IOA f' cf _   _fo :: IOA cf     fas fo)
     ,Just (IOA t' _  tas _to :: IOA ct tas to))
      -> Right (IOA ioa cf tas (Proxy @ro) :: IOA cf tas ro)
     where
       ioa :: Result (ReprOf ro)
       ioa = traversePipes0 (descOutTag dt) (descOutType dt) (Proxy @b) f' t'
    (_, Nothing) -> Left . pack $ printf
      "travDyn: invariant failure: tas %s, to %s, dyn %s"
      (show $ typeRep @tas) (show $ typeRep @to) (show $ dynRep f)
    (Nothing, _) -> Left . pack $ printf
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
traversePipes0
  :: forall ttr tr fr
  . ( Typeable tr, Typeable fr
    )
  -- TODO:  see if we can just pass a TypePair here
  => Tag ttr -> Proxy tr -> Proxy fr
  -> (Repr Point tr -> Result (Repr Point fr))
  -> Result (Repr ttr tr)
  -> Result (Repr ttr fr)
traversePipes0 ttr _ _ f t = do
  tv <- t
  case tv :: Either Text (Repr ttr tr) of
    Left e -> pure $ Left e
    Right (x :: Repr ttr tr) ->
      case ttr of
        TPoint -> pure $ Left "traverse: asked to traverse a Point"
        TList  -> sequence <$> traverse f x
        TSet   -> pure $ Left "traverse: Set unsupported"
        TTree  -> pure $ Left "traverse: Tree unsupported"
        TDag   -> pure $ Left "traverse: Dag unsupported"
        TGraph -> pure $ Left "traverse: Graph unsupported"

traversePipes1
  :: forall tta ta ttr tr fr
  . ( Typeable ta, Typeable tr, Typeable fr
    )
  => Tag tta -> Proxy ta -> Tag ttr -> Proxy tr -> Proxy fr
  -> (tr -> Result fr)
  -> (Repr tta ta -> Result (Repr ttr tr))
  -> (Repr tta ta -> Result (Repr ttr fr))
traversePipes1 _ _ kb _ _ f t = \ra -> do
  tv <- t ra
  case tv :: Either Text (Repr ttr tr) of
    Left e -> pure $ Left e
    Right (x :: Repr ttr tr) ->
      case kb of
        TPoint -> f x
        TList  -> sequence <$> traverse f x
        TSet   -> pure $ Left "traverse: Set unsupported"
        TTree  -> pure $ Left "traverse: Tree unsupported"
        TDag   -> pure $ Left "traverse: Dag unsupported"
        TGraph -> pure $ Left "traverse: Graph unsupported"
