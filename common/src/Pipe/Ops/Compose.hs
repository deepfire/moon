{-# LANGUAGE PartialTypeSignatures #-}
module Pipe.Ops.Compose
  ( -- compose
  -- ,
    demoCompose
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.SOP                         as SOP
import           Type.Reflection

import Basis
import Pipe.Types
import Pipe.Ops.Base
import Pipe.Ops.Internal
import Type

demoCompose :: IO ()
demoCompose = case compose compDyn pipe val of
  Left e -> putStrLn . unpack $ "compose error: " <> e
  Right p -> runPipe p >>= \case
    Left e -> putStrLn . unpack $ "runtime error: " <> e
    Right _ -> pure ()
 where
   pipe :: SomePipe Dynamic
   pipe = linkG "demo pipe" TPoint' TPoint'
     ((>> pure (Right ())) . putStrLn . (<> " (c)(r)(tm)"))

   val :: SomePipe Dynamic
   val = genG "demo value" TPoint'
     (pure $ Right ("demo!" :: String))


-- | 'compose': approximate '(.)':
-- (.) :: (b -> c) -> (a -> b) -> a -> c
compose
  :: (forall cf cv vas vo fas fass ras fo
      . ( PipeConstr cv vas vo
        , PipeConstr cf fas fo
        , fas ~ (vo:fass)
        , ras ~ fass
        )
      => (Desc cv vas vo -> p -> Desc cf fas fo -> p -> Either Text p))
  -> SomePipe p
  -> SomePipe p
  -> Either Text (SomePipe p)
compose pf f v =
  somePipeUncons f
  (const "Cannot apply value to a saturated pipe.") $
  -- • Could not deduce: (ka :: *) ~ (Type (TagOf ka1) (TypeOf ka1) :: *)
  -- Expected type: Pipe c kas o p
  --   Actual type: Pipe c ((':) * ka kas') o p
  -- ka is
  -- ka1 is
  --   from the context: (PipeConstr c kas o, PipeConstr c kas' o,
  --                     (kas :: [*]) ~ ((':) * ka1 kas' :: [*]))
  --   bound by a type expected by the context:
  -- forall (c :: * -> Constraint) o (kas :: [*]) ka1 (kas' :: [*]).
  -- (PipeConstr c kas o, PipeConstr c kas' o,
  --  (kas :: [*]) ~ ((':) * ka1 kas' :: [*])) =>
  -- Pipe c kas o p -> Either Text (Pipe c kas' o p)
  -- Expected type: Pipe c kas o p
  --   Actual type: Pipe c ((':) * ka kas') o p
  \(f' :: Pipe cf (ka : kas) fo p) ->
    withSomePipe v $
    \(v' :: Pipe cv _vas vo p) ->
    case typeRep @ka  `eqTypeRep` typeRep @vo of
      Just HRefl -> compose'' pf f' v'
      _ -> error "compose"

compose''
  :: forall cf cv vas vo fas fass ras fo p
  . ( PipeConstr cv vas vo
    , PipeConstr cf fas fo
    , fas ~ (vo:fass)
    , ras ~ fass
    )
  => (forall cf cv vas vo fas fass ras fo
       . ( PipeConstr cv vas vo
         , PipeConstr cf fas fo
         , fas ~ (vo:fass)
         , ras ~ fass
         )
      => Desc cv vas vo -> p -> Desc cf fas fo -> p -> Either Text p)
  -> Pipe cf fas fo p
  -> Pipe cv vas vo p
  -> Either Text (Pipe cf ras fo p)
compose'' pf
  -- | Just HRefl <- typeRep @kt2 `eqTypeRep` typeRep @kf1
  -- , Just HRefl <- typeRep @tt2 `eqTypeRep` typeRep @tf1
  | True
  = compose' pf
  | otherwise
  = \l r -> Left $ "'compose' failed on: l="<>pack (show l)<>" r="<>pack (show r)

-- | 'compose': approximate '(.)'
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ..and convert to
-- (=<<) :: (a -> m b) -> m a -> m b
compose'
  :: forall cf cv vas vo fas fass ras fo p
   . ( PipeConstr cv vas vo
     , PipeConstr cf fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => (Desc cv vas vo -> p -> Desc cf fas fo -> p -> Either Text p)
  -> Pipe cf fas fo p
  -> Pipe cv vas vo p
  -> Either Text (Pipe cf ras fo p)
compose' composeF
  pF@P{pPipeRep=frep@IOATyCons{}}
  pV@P{pPipeRep=vrep@IOATyCons{}}
  | Just e <- ioaTyConsInvalidity frep
  = Left $ "Compose: sink function: " <> e
  | Just e <- ioaTyInvalidity vrep
  = Left $ "Compose: source function: " <> e

  | otherwise
  = doBind composeF pF pV
compose' _ f v
  = Left $ "compose: incompatible: f="<>pack (show f)<>" v="<>pack (show v)

-- | 'doBind': approximate 'bind':
-- (=<<) :: (a -> m b) -> m a -> m b
-- ..with the difference that we should handle a FLink on the left as well,
-- ..but not just yet.
doBind
  :: forall cf cv vr fr fas fass ras fo vas vo p
   . ( Typeable cf
     , PipeConstr cv vas vo, vr ~ ReprOf vo
     , PipeConstr cf fas fo, fr ~ ReprOf fo
     -- this is hard-coded to a 0-ary function being applied to a 1-ary one
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => (Desc cv vas vo     -> p ->
      Desc cf     fas fo -> p ->
      Either Text p)
  -> Pipe cf        fas fo p
  -> Pipe cv vas vo        p
  -> Either Text (Pipe cf fass fo p)
doBind pf
  P{ pDesc_=df, pName=Name fn, pArgStys=sfas, pOutSty=sfo, pStruct=Struct fg
   , pArgs=(_fa SOP.:* fass), pOut=fo, pPipe=f}
  P{ pDesc_=dv, pName=Name vn, pStruct=Struct vg
   , pOut=_vo, pPipe=v}
  = Pipe desc <$> pf dv v df f
 where
   desc    = Desc name sig struct (SomeTypeRep rep) fass fo
   name    = Name $ fn<>">>="<>vn
   sig     = Sig (tail sfas) sfo
   struct  = Struct $ G.overlay fg vg
   rep     = case spineConstraint of
               (Dict :: Dict Typeable fass) ->
                 typeRep :: TypeRep (IOA cf fass fo)
doBind _ _ _ = error "doBind"

compDyn
  :: forall cf cv vas vo fas fass ras fo
   . ( PipeConstr cv vas vo
     , PipeConstr cf fas fo
     , fas ~ (vo:fass)
     , ras ~ fass
     )
  => Desc cv vas vo     -> Dynamic
  -> Desc cf     fas fo -> Dynamic
  -> Either Text Dynamic
compDyn Desc{pdArgs=pdArgsV} vIOADyn Desc{pdArgs=pdArgsF} fIOADyn =
  case spineConstraint of
    (Dict :: Dict Typeable vas) ->
      case spineConstraint of
        (Dict :: Dict Typeable fas) ->
          case ( fromDynamic vIOADyn
               , fromDynamic fIOADyn) of
            ( Just (IOA v' _cv _asv _vo :: IOA cv vas vo)
             ,Just (IOA f'  cf _asf  fo :: IOA cf fas fo)) ->
              -- NOTE:  not having implemented the general case,
              --        we essentially had two options here:
              --  1. pass type-level proof of the limited-case arguments top-down
              --  2. recover it later
              --  We opt for 2 here.
              case (pdArgsV, pdArgsF) of
                (Nil, _ :* Nil) -> Right $ Dynamic typeRep
                  -- just a monadic value:
                  (IOA (bindPipes0 v' f') cf (Proxy @ras) fo
                       :: IOA cf ras fo)
                -- XXX: tough..
                -- (_ :: TypePair va) :* Nil -> Right $ Dynamic typeRep
                --   (IOA (bindPipes1 v' f')
                --        (Proxy @cf)
                --        (Proxy @(va : ras))
                --        fo
                --        :: IOA cf (va : ras) fo)
                _ -> Left . pack $ printf
                     "compDyn: unhandled value arity >f: kasf %s"
                     (show $ typeRep @vas)
            _ ->
              Left . pack $ printf
              "compDyn: invariant failure: vas %s, vo %s, vdyn %s, (of:fass) %s, of %s, fdyn %s"
              (show $ typeRep @vas) (show $ typeRep @vo) (show $ dynRep vIOADyn)
              (show $ typeRep @fas) (show $ typeRep @fo) (show $ dynRep fIOADyn)

-- | 'bindPipes*': approximate 'bind':
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b
bindPipes0
  :: Result b
  -> (b -> Result c)
  -> Result c
bindPipes0 v f = do
  -- fmap join . join $ traverse f <$> v
  r <- v
  case r of
    Left e  -> pure $ Left e
    Right x -> f x

_bindPipes1
  :: (a -> Result b)
  -> (b -> Result c)
  -> (a -> Result c)
_bindPipes1 v f ra = do
  -- fmap join . join $ traverse f <$> v ra
  r <- v ra
  case r of
    Left e  -> pure $ Left e
    Right x -> f x
