{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
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
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Result
import Dom.Sig
import Dom.SomeType
import Dom.Struct
import Dom.Tags
import Dom.Value
import Dom.VTag


--------------------------------------------------------------------------------
-- * Guts of the pipe guts.
--
type family PipeFunTy (l :: Liveness) (as :: [*]) (o :: *) :: * where
  PipeFunTy l '[]    o = Result l (ReprOf o)
  PipeFunTy l (x:xs) o = ReprOf x -> PipeFunTy l xs o

data IOA (l :: Liveness) (as :: [*]) (o :: *) where
  IOA :: PipeConstr Now as o
      => PipeFunTy Now as o
      -> Proxy as
      -> Proxy o
      -> IOA Now as o
  IOE :: PipeConstr (Live t m) as o
      => PipeFunTy (Live t m) as o
      -> Proxy t
      -> Proxy m
      -> Proxy as
      -> Proxy o
      -> IOA (Live t m) as o

--------------------------------------------------------------------------------
-- * Running pipes
--
runIOADynamic ::
  forall (l :: Liveness) c v o
  . (o ~ CTagV c v
    , Typeable c, Typeable v
    , LiveConstr l)
  => Dynamic.Dynamic
  -> LTag l -> CTag c -> VTag v
  -> Result l (Value c v)
runIOADynamic dyn LNow c v =
  case Dynamic.fromDynamic dyn :: Maybe (IOA Now '[] o) of
    Nothing -> fallM $ "Not a runnable Now IOA: " <>
               showSomeTypeRepNoKind (dynRep dyn)
    Just (IOA ioa _as _o) ->
      (mkValue c v <$>) <$> ioa
runIOADynamic dyn l@LLive{} c v =
  case l of
    (LLive trt trm :: LTag (Live t m)) ->
      case withTypeable trt $
           Dynamic.fromDynamic dyn :: Maybe (IOA (Live t m) '[] o) of
        Nothing -> pure never
                   -- fallM $ "Not a runnable Live IOE: " <>
                   --          showSomeTypeRepNoKind (dynRep dyn)
        Just (IOE ioa _t _m _as _o) ->
          fmap (mkValue c v <$>) <$> ioa

--------------------------------------------------------------------------------
-- * Implementation of the high-level ops
--
appDyn ::
     forall l as ass (o :: *) a c v
   . ( PipeConstr l as o
     , as ~ (a:ass)
     , a ~ CTagV c v
     )
  => Desc l as o -> Value (CTagVC a) (CTagVV a) -> Dynamic.Dynamic
  -> Fallible Dynamic.Dynamic
appDyn Desc {pdArgs = Tags _ _ SOP.:* _} v ioaDyn =
  case spineConstraint of
    (Dict :: Dict Typeable ass) ->
      Dynamic typeRep <$>
        case Dynamic.fromDynamic ioaDyn of
          Nothing -> fallS $ printf
            "appDyn: invariant failure: as %s, o %s, dyn %s"
            (show $ typeRep @as) (show $ typeRep @o) (show $ dynRep ioaDyn)
          Just (IOA (f :: PipeFunTy Now (CTagV c v:ass) o) _as o :: IOA Now as o) ->
            Right $ IOA (applyPipeFun' LNow f (Proxy @ass) o v :: PipeFunTy Now ass o)
                        (Proxy @ass) (Proxy @o)

travDyn ::
     forall l tas to tt a b fas fo ro
   . ( PipeConstr l fas fo
     , PipeConstr l tas to
     , fas ~ (CTagV Point a ': '[])
     , tas ~ '[]
     , fo  ~ CTagV Point b
     , to  ~ CTagV tt    a
     , ro  ~ CTagV tt    b
     )
  => Desc l     fas fo -> Dynamic.Dynamic
  -> Desc l tas to     -> Dynamic.Dynamic
  -> Fallible Dynamic.Dynamic
travDyn _df f dt t = Dynamic typeRep <$>
  case (Dynamic.fromDynamic f, Dynamic.fromDynamic t) of
    (_, Nothing) -> fallS $ printf
      "travDyn: invariant failure: tas %s, to %s, dyn %s"
      (show $ typeRep @tas) (show $ typeRep @to) (show $ dynRep f)
    (Nothing, _) -> fallS $ printf
      "travDyn: invariant failure: fas %s, fo %s, dyn %s"
      (show $ typeRep @fas) (show $ typeRep @fo) (show $ dynRep t)
    ( Just (IOA f' _   _fo :: IOA Now     fas fo)
     ,Just (IOA t' tas _to :: IOA Now tas to))
      -> Right $ IOA (traversePipes0 LNow (descOutCTag dt) (descOutVTag dt)
                                     (Proxy @b) f' t')
                     tas (Proxy @ro)

compDyn ::
     forall l vas vo fas fass ras fo
   . ( PipeConstr l vas vo
     , PipeConstr l fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => Desc l vas vo     -> Dynamic.Dynamic
  -> Desc l     fas fo -> Dynamic.Dynamic
  -> Fallible Dynamic.Dynamic
compDyn Desc{pdArgs=pdArgsV} vIOADyn Desc{pdArgs=pdArgsF} fIOADyn =
  case (spineConstraint, spineConstraint) of
    (Dict :: Dict Typeable vas,
     Dict :: Dict Typeable fas) ->
      case ( Dynamic.fromDynamic vIOADyn
           , Dynamic.fromDynamic fIOADyn) of
        ( Just (IOA v' _asv _vo :: IOA Now vas vo)
         ,Just (IOA f' _asf  fo :: IOA Now fas fo)) ->
          -- NOTE:  not having implemented the general case,
          --        we essentially had two options here:
          --  1. pass type-level proof of the limited-case arguments top-down
          --  2. recover it later
          --  We opt for 2 here.
          case (pdArgsV, pdArgsF) of
            (Nil, _ :* Nil) -> Right $ Dynamic typeRep
              -- just a monadic value:
              (IOA (bindPipes0 v' f') (Proxy @ras) fo
                   :: IOA Now ras fo)
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
     Result Now b
  -> (b -> Result Now c)
  -> Result Now c
bindPipes0 v f = do
  -- fmap join . join $ traverse f <$> v
  r <- v
  case r of
    Left e  -> pure $ Left e
    Right x -> f x

-- _bindPipes1 ::
--      (a -> Result t l b)
--   -> (b -> Result t l c)
--   -> (a -> Result t l c)
-- _bindPipes1 v f ra = do
--   -- fmap join . join $ traverse f <$> v ra
--   r <- v ra
--   case r of
--     Left e  -> pure $ Left e
--     Right x -> f x

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
    forall l ttr tr fr
  . ( Typeable tr, Typeable fr
    )
  -- TODO:  see if we can just pass a Tags here
  => LTag l -> CTag ttr -> VTag tr -> Proxy fr
  -> (Repr Point tr -> Result l (Repr Point fr))
  -> Result l (Repr ttr tr)
  -> Result l (Repr ttr fr)
traversePipes0 LNow ttr _ _ f t = do
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
    forall l tta ta ttr tr fr
  . ( Typeable ta, Typeable tr, Typeable fr
    )
  => LTag l -> CTag tta -> Proxy ta -> CTag ttr -> Proxy tr -> Proxy fr
  -> (tr -> Result l fr)
  -> (Repr tta ta -> Result l (Repr ttr tr))
  -> (Repr tta ta -> Result l (Repr ttr fr))
traversePipes1 LNow _ _ cb _ _ f t = \ra -> do
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
  forall (l :: Liveness) (as :: [*]) (o :: *) (c :: Con) (a :: *)
  .  LTag  l
  -> PipeFunTy l (CTagV c a:as) o
  -> Proxy as
  -> Proxy o
  -> Value c a
  -> PipeFunTy l as o
applyPipeFun' LNow f _ _ = \case
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
-- * Destructuring pipes
--
pattern P
  :: Desc l as o -> Name Pipe -> Struct -> SomeTypeRep -> p
  -> [SomeType]   -> SomeType
  -> LTag l
  -> NP Tags as -> Tags o
  -> Pipe l as o p
pattern P { pDesc_, pName, pStruct, pPipeRep, pPipe, pArgStys, pOutSty, pLiveness, pArgs, pOut }
  <- Pipe pDesc_@(Desc pName (Sig (fmap unI -> pArgStys) (I pOutSty)) pStruct
                  pPipeRep pLiveness pArgs pOut)
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
