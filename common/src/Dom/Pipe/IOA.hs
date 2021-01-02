module Dom.Pipe.IOA (module Dom.Pipe.IOA) where

import Data.Dynamic                     qualified as Dynamic
import Data.SOP                         qualified as SOP
import Type.Reflection
         ( pattern App
         , pattern Con
         , TyCon
         , someTypeRepTyCon
         , splitApps
         , typeRepTyCon
         )

import Reflex

import Basis

import Dom.CTag
import Dom.Error
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Sig
import Dom.SomeType
import Dom.Struct
import Dom.Tags
import Dom.Value
import Dom.VTag


--------------------------------------------------------------------------------
-- * Guts of the pipe guts.
--
type family PipeFunTy (as :: [*]) (o :: *) :: * where
  PipeFunTy '[]    o = Result (ReprOf o)
  PipeFunTy (x:xs) o = ReprOf x -> PipeFunTy xs o

data IOA (as :: [*]) (o :: *) where
  IOA :: PipeConstr as o
      => PipeFunTy as o
      -> Proxy as
      -> Proxy o
      -> IOA as o

--------------------------------------------------------------------------------
-- * Implementation of the high-level ops
--
appDyn ::
     forall as ass (o :: *) a c v
   . ( PipeConstr as o
     , as ~ (a:ass)
     , a ~ CTagV c v
     )
  => Desc as o -> Value (CTagVC a) (CTagVV a) -> Dynamic.Dynamic
  -> Fallible Dynamic.Dynamic
appDyn Desc {pdArgs = Tags _ _ SOP.:* _} v ioaDyn =
  case spineConstraint of
    (Dict :: Dict Typeable ass) ->
      Dynamic typeRep <$>
        case Dynamic.fromDynamic ioaDyn of
          Nothing -> fallS $ printf
            "appDyn: invariant failure: as %s, o %s, dyn %s"
            (show $ typeRep @as) (show $ typeRep @o) (show $ dynRep ioaDyn)
          Just (IOA (f :: PipeFunTy (CTagV c v:ass) o) _as o :: IOA as o) ->
            Right $ IOA (applyPipeFun' f (Proxy @ass) o v :: PipeFunTy ass o)
                        (Proxy @ass) (Proxy @o)

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
  => Desc     fas fo -> Dynamic.Dynamic
  -> Desc tas to     -> Dynamic.Dynamic
  -> Fallible Dynamic.Dynamic
travDyn _df f dt t = Dynamic typeRep <$>
  case (Dynamic.fromDynamic f, Dynamic.fromDynamic t) of
    (_, Nothing) -> fallS $ printf
      "travDyn: invariant failure: tas %s, to %s, dyn %s"
      (show $ typeRep @tas) (show $ typeRep @to) (show $ dynRep f)
    (Nothing, _) -> fallS $ printf
      "travDyn: invariant failure: fas %s, fo %s, dyn %s"
      (show $ typeRep @fas) (show $ typeRep @fo) (show $ dynRep t)
    ( Just (IOA f' _   _fo :: IOA     fas fo)
     ,Just (IOA t' tas _to :: IOA tas to))
      -> Right $ IOA (traversePipes0 (descOutCTag dt) (descOutVTag dt)
                                     (Proxy @b) f' t')
                     tas (Proxy @ro)

compDyn ::
     forall vas vo fas fass ras fo
   . ( PipeConstr vas vo
     , PipeConstr fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => Desc vas vo     -> Dynamic.Dynamic
  -> Desc     fas fo -> Dynamic.Dynamic
  -> Fallible Dynamic.Dynamic
compDyn Desc{pdArgs=pdArgsV} vIOADyn Desc{pdArgs=pdArgsF} fIOADyn =
  case (spineConstraint, spineConstraint) of
    (Dict :: Dict Typeable vas,
     Dict :: Dict Typeable fas) ->
      case ( Dynamic.fromDynamic vIOADyn
           , Dynamic.fromDynamic fIOADyn) of
        ( Just (IOA v' _asv _vo :: IOA vas vo)
         ,Just (IOA f' _asf  fo :: IOA fas fo)) ->
          -- NOTE:  not having implemented the general case,
          --        we essentially had two options here:
          --  1. pass type-level proof of the limited-case arguments top-down
          --  2. recover it later
          --  We opt for 2 here.
          case (pdArgsV, pdArgsF) of
            (Nil, _ :* Nil) -> Right $ Dynamic typeRep
              -- just a monadic value:
              (IOA (bindPipes0 v' f') (Proxy @ras) fo
                   :: IOA ras fo)
            -- XXX: tough..
            -- (_ :: TypePair va) :* Nil -> Right $ Dynamic typeRep
            --   (IOA (bindPipes1 v' f')
            --        (Proxy @cf)
            --        (Proxy @(va : ras))
            --        fo
            --        :: IOA cf (va : ras) fo)
            _ -> fallS $ printf
                 "compDyn: unhandled value arity >1: casf %s"
                 (show $ typeRep @vas)
        _ ->
          fallS $ printf
          "compDyn: invariant failure: vas %s, vo %s, vdyn %s, (of:fass) %s, of %s, fdyn %s"
          (show $ typeRep @vas) (show $ typeRep @vo) (show $ dynRep vIOADyn)
          (show $ typeRep @fas) (show $ typeRep @fo) (show $ dynRep fIOADyn)

--------------------------------------------------------------------------------
-- * Plumbing for the implementation of the high-level ops
--
-- | 'bindPipes*': approximate 'bind':
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b
bindPipes0 ::
     Result b
  -> (b -> Result c)
  -> Result c
bindPipes0 v f = do
  -- fmap join . join $ traverse f <$> v
  r <- v
  case r of
    Left e  -> pure $ Left e
    Right x -> f x

_bindPipes1 ::
     (a -> Result b)
  -> (b -> Result c)
  -> (a -> Result c)
_bindPipes1 v f ra = do
  -- fmap join . join $ traverse f <$> v ra
  r <- v ra
  case r of
    Left e  -> pure $ Left e
    Right x -> f x

-- XXX: where is the best place for this check now?
-- doTraverse l _ _ r _ _
--   = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

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

-- | 'applyPipeFun': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
applyPipeFun' ::
  forall (as :: [*]) (o :: *) (c :: Con) (a :: *)
  .  PipeFunTy (CTagV c a:as) o
  -> Proxy as
  -> Proxy o
  -> Value c a
  -> PipeFunTy as o
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

--------------------------------------------------------------------------------
-- * Running pipes
--
runIOADynamic ::
  forall c v o
  . (o ~ CTagV c v, Typeable c, Typeable v)
  => Dynamic.Dynamic
  -> CTag c -> VTag v
  -> Result (Value c v)
runIOADynamic dyn c v =
  case Dynamic.fromDynamic dyn :: Maybe (IOA '[] o) of
    Nothing -> fallM $ "Not a runnable Ground IOA: " <>
                        showSomeTypeRepNoKind (dynRep dyn)
    Just ioa ->
      runIOA ioa c v

runIOA ::
  (ReprOf o ~ Repr c v, HasCallStack)
  => IOA '[] o
  -> CTag c -> VTag v
  -> Result (Value c v)
runIOA (IOA io _as _o) c v =
  (mkValue c v <$>) <$> io

--------------------------------------------------------------------------------
-- * Destructuring pipes
--
pattern P
  :: Desc as o -> Name Pipe -> Struct -> SomeTypeRep -> p
  -> [SomeType]   -> SomeType
  -> NP Tags as -> Tags o
  -> Pipe as o p
pattern P { pDesc_, pName, pStruct, pPipeRep, pPipe, pArgStys, pOutSty, pArgs, pOut }
  <- Pipe pDesc_@(Desc pName (Sig (fmap unI -> pArgStys) (I pOutSty)) pStruct
                  pPipeRep pArgs pOut)
          pPipe

pattern IOATyCons
  :: TyCon -> TyCon -- -> TypeRep c
  -> TyCon -> TypeRep ka -> TypeRep a
  -> TypeRep rest
  -> TyCon -> TypeRep ko -> TypeRep o
  -> SomeTypeRep
pattern IOATyCons
  { ioaCon, listCon --, constrRep
  , typeACon, tagARep, aRep
  , restRep
  , typeOCon, tagORep, oRep
  }
  <- SomeTypeRep (App
                  (App --(App (Con ioaCon) (Con _constrRep))
                       (Con ioaCon)
                       (App (App (Con listCon)
                                 (App (App (Con typeACon) tagARep)
                                      aRep))
                            restRep))
                  (App (App (Con typeOCon) tagORep)
                       oRep))

pattern IOATyNil
  :: TyCon -> TyCon -> TyCon -> TypeRep ko -> TypeRep o -> SomeTypeRep
pattern IOATyNil ioaCon nilCon typeOCon tagORep oRep
  <- SomeTypeRep (App
                  (App -- (App (Con ioaCon) (Con _cstr))
                       (Con ioaCon)
                       (Con nilCon))
                  (App (App (Con typeOCon) tagORep)
                       oRep))

--------------------------------------------------------------------------------
-- * RTTI tools
--
typeRepNull
  :: forall k (a :: [k]) (b :: [k])
   . (Typeable k, b ~ '[])
  => TypeRep a
  -> Maybe (a :~~: b)
typeRepNull rep = rep `eqTypeRep` typeRep @('[] :: [k])

consTyCon, ioaTyCon, nilTyCon, typeTyCon :: TyCon
consTyCon = typeRepTyCon (typeRep @(() : '[]))
nilTyCon  = someTypeRepTyCon (head $ tail $ snd $ splitApps $ typeRep @(() : '[]))
typeTyCon = typeRepTyCon (typeRep @CTagV)
ioaTyCon = typeRepTyCon (typeRep @IOA)

ioaTyInvalidity :: SomeTypeRep -> Maybe Text
ioaTyInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= ioaTyCon  = Just "not an IOA"
  | lcon /= nilTyCon &&
    lcon /= consTyCon = Just ("arglist type not a list: " <> pack (show lcon))
  | ocon /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyInvalidity _     = Just "no match with an IOA"

ioaTyConsInvalidity :: SomeTypeRep -> Maybe Text
ioaTyConsInvalidity IOATyCons{ioaCon=ioa, listCon=lst, typeACon=tyA, typeOCon=tyO}
  |   ioa /= ioaTyCon  = Just "not an IOA"
  |   lst /= consTyCon = Just "arglist type not a nonempty list"
  | tyA   /= typeTyCon = Just "first arg not a Type"
  | tyO   /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyConsInvalidity _ = Just "no match with IOATyCons"

ioaTySingletonInvalidity :: SomeTypeRep -> Maybe Text
ioaTySingletonInvalidity rep@IOATyCons{}
  | Just e <- ioaTyConsInvalidity rep = Just e
  | otherwise = case rep of
      IOATyCons{restRep=Con{}} -> Nothing
      _ -> Just "arglist type not a singleton list"
ioaTySingletonInvalidity _ = Just "arglist type not a singleton list"

ioaTyNilInvalidity :: SomeTypeRep -> Maybe Text
ioaTyNilInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= ioaTyCon  = Just "not an IOA"
  | lcon /= nilTyCon  = Just "arglist type not an empty list"
  | ocon /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyNilInvalidity _  = Just "no match with IOATyNil"
