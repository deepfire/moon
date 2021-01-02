{-# LANGUAGE PartialTypeSignatures #-}
module Dom.Pipe.Ops.Compose (module Dom.Pipe.Ops.Compose) where

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
import Dom.Struct

import Ground.Table() -- for demo only


--------------------------------------------------------------------------------
-- * Showcase
--
demoCompose :: IO ()
demoCompose = case compose compDyn pipe val of
  Left e -> putStrLn $ show e
  Right p -> runSomePipe p >>= \case
    Left e -> putStrLn . unpack $ "runtime error: " <> showError e
    Right _ -> pure ()
 where
   pipe :: SomePipe Dynamic
   pipe = somePipe1 "demo pipe" capsT CVPoint CVPoint
     ((>> pure (Right ())) . putStrLn . (<> " (c)(r)(tm)"))

   val :: SomePipe Dynamic
   val = somePipe0 "demo value" capsT CVPoint
     (pure $ Right ("compose!" :: String))

--------------------------------------------------------------------------------
-- * Conceptually:
--
-- compose ~:: (b -> c) -> (a -> b) -> a -> c
--
compose ::
     (forall vas vo fas fass fo
      . ( PipeConstr vas vo
        , PipeConstr fas fo
        , fas ~ (vo:fass)
        )
      => (Desc vas vo -> p -> Desc fas fo -> p -> Fallible p))
  -> SomePipe p
  -> SomePipe p
  -> PFallible (SomePipe p)
compose pf f v =
  somePipeUncons f
  (const $ EComp "Cannot compose value and a saturated pipe.") $
  \(f' :: Pipe (a : as) fo p) ->
    withSomePipe v $
    \(v' :: Pipe _vas vo p) ->
      case typeRep @a  `eqTypeRep` typeRep @vo of
        Just HRefl -> left EComp $ compose'' pf f' v'
        _ -> error "compose"

compose'' ::
    forall vas vo fas fass ras fo p
  . ( PipeConstr vas vo
    , PipeConstr fas fo
    , fas ~ (vo:fass)
    , ras ~ fass
    )
  => (forall vas' vo' fas' fass' ras' fo'
       . ( PipeConstr vas' vo'
         , PipeConstr fas' fo'
         , fas' ~ (vo':fass')
         , ras' ~ fass'
         )
      => Desc vas' vo' -> p -> Desc fas' fo' -> p -> Fallible p)
  -> Pipe fas fo p
  -> Pipe vas vo p
  -> Fallible (Pipe ras fo p)
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
     forall vas vo fas fass ras fo p
   . ( PipeConstr vas vo
     , PipeConstr fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => (Desc vas vo -> p -> Desc fas fo -> p -> Fallible p)
  -> Pipe fas fo p
  -> Pipe vas vo p
  -> Fallible (Pipe ras fo p)
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
     forall vr fr fas fass ras fo vas vo p
   . ( PipeConstr vas vo, vr ~ ReprOf vo
     , PipeConstr fas fo, fr ~ ReprOf fo
     -- this is hard-coded to a 0-ary function being applied to a 1-ary one
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => (Desc vas vo     -> p ->
      Desc     fas fo -> p ->
      Fallible p)
  -> Pipe        fas fo p
  -> Pipe vas vo        p
  -> Fallible (Pipe fass fo p)
doBind pf
  P{ pDesc_=df, pName=Name fn, pArgStys=sfas, pOutSty=sfo, pStruct=Struct fg
   , pArgs=(_fa SOP.:* fass), pOut=fo, pPipe=f}
  P{ pDesc_=dv, pName=Name vn, pStruct=Struct vg
   , pOut=_vo, pPipe=v}
  = Pipe desc <$> pf dv v df f
 where
   desc    = Desc name sig struct (SomeTypeRep rep) fass fo
   name    = Name $ "("<>fn<>") >>= ("<>vn<>")"
   sig     = Sig (I <$> tail sfas) (I sfo)
   struct  = Struct $ G.overlay fg vg
   rep     = case spineConstraint of
               (Dict :: Dict Typeable fass) ->
                 typeRep :: TypeRep (IOA fass fo)
doBind _ _ _ = error "doBind"

compDyn ::
     forall vas vo fas fass ras fo
   . ( PipeConstr vas vo
     , PipeConstr fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => Desc vas vo     -> Dynamic
  -> Desc     fas fo -> Dynamic
  -> Fallible Dynamic
compDyn Desc{pdArgs=pdArgsV} vIOADyn Desc{pdArgs=pdArgsF} fIOADyn =
  case spineConstraint of
    (Dict :: Dict Typeable vas) ->
      case spineConstraint of
        (Dict :: Dict Typeable fas) ->
          case ( fromDynamic vIOADyn
               , fromDynamic fIOADyn) of
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
                     "compDyn: unhandled value arity >f: casf %s"
                     (show $ typeRep @vas)
            _ ->
              fallS $ printf
              "compDyn: invariant failure: vas %s, vo %s, vdyn %s, (of:fass) %s, of %s, fdyn %s"
              (show $ typeRep @vas) (show $ typeRep @vo) (show $ dynRep vIOADyn)
              (show $ typeRep @fas) (show $ typeRep @fo) (show $ dynRep fIOADyn)

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
