module Pipe.Ops.Base
  ( runPipe
  , gen,  genG, gen'
  , link, linkG, link'
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)

import Basis
import Pipe.Ops.Internal
import Pipe.Types
import Type


-- * Running
--
runPipe :: SomePipe Dynamic -> Result SomeValue
runPipe (T p) = pure . Left $ "runPipe:  non-Ground pipe: " <> showPipe p
runPipe (G Pipe{pDesc, p}) = runPipe' pDesc p

runPipe'
  :: forall c (as :: [*]) o. PipeConstr c as o
  => Desc c as o
  -> Dynamic
  -> Result SomeValue
runPipe' pd@Desc{pdOut=TypePair{tpTag, tpType}} dyn =
  case fromDynamic dyn :: Maybe (IOA Ground '[] o) of
    Nothing -> pure . Left $ "Incomplete pipe: " <> showDesc pd
    Just (IOA io _c _as _o) ->
      (SomeValue . SomeKindValue tpTag . mkValue tpType tpTag <$>) <$> io


-- * Constructors
--
genG
  :: forall kt tt
  .  (ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Ground tt)
  => Name Pipe
  -> Type kt tt
  -> Result (Repr kt tt)
  -> SomePipe Dynamic
genG n to pf = G $ gen' n to pf

gen
  :: forall kt tt
  .  (ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Typeable tt)
  => Name Pipe
  -> Type kt tt
  -> Result (Repr kt tt)
  -> SomePipe Dynamic
gen n to pf = T $ gen' n to pf

linkG
  :: forall kf tf kt tt
  . ( ReifyTag kf,ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt
    , Ground tt)
  => Name Pipe
  -> Type kf tf
  -> Type kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> SomePipe Dynamic
linkG n from to pf = G $ link' n from to pf

link
  :: forall kf tf kt tt
  . ( ReifyTag kf, ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt
    , Typeable tt)
  => Name Pipe
  -> Type kf tf
  -> Type kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> SomePipe Dynamic
link n from to pf = T $ link' n from to pf

gen'
  :: forall kf tf kt tt c
  .  ( kf ~ 'Point, tf ~ ()
     , ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Typeable tt, Typeable c
     , c tt)
  => Name Pipe
  -> Type kt tt
  -> Result (Repr kt tt)
  -> Pipe c '[] (Type kt tt) Dynamic
gen' name (splitType -> (tagTo, pTo)) mv
  -- TODO: validate types against the typerep/dynamic
                = Pipe desc dyn
  where ty      = tagSomeType tagTo pTo
        desc    = Desc name sig struct (dynRep dyn) Nil (TypePair tagTo pTo)
        sig     = Sig [] ty
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mv Proxy Proxy Proxy :: IOA c '[] (Type kt tt)

link'
  :: forall kf tf kt tt c
  . ( ReifyTag kf, ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt, Typeable tt, Typeable c
    , c tt)
  => Name Pipe
  -> Type kf tf
  -> Type kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> Pipe c '[Type kf tf] (Type kt tt) Dynamic
link' name (splitType -> (kf, tf)) (splitType -> (kt, tt)) mf
                = Pipe desc dyn
  where desc    = Desc name sig struct (dynRep dyn) (TypePair kf tf :* Nil) (TypePair kt tt)
        sig     = Sig [tagSomeType kf tf] (tagSomeType kt tt)
        struct  = Struct G.empty
          -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)
        ---------
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mf Proxy Proxy Proxy :: IOA c '[Type kf tf] (Type kt tt)

-- emptyDesc :: Name Pipe -> Desc Ground '[] ()
-- emptyDesc name = Desc
--   { pdName   = name
--   , pdSig    = Gen unitType unitType
--   , pdStruct = Struct G.empty
--   , pdRep    = SomeTypeRep (typeRep @(IOA Ground '[Type Point ()] (Type Point ())))
--   , pdArgs   = Nil
--   , pdOut    = Proxy @()
--   }

-- emptyPipe :: Name Pipe -> p -> Pipe Ground '[Type Point ()] (Type Point ()) p
-- emptyPipe = Pipe . emptyDesc

-- someEmptyPipe :: Name Pipe -> p -> SomePipe p
-- someEmptyPipe = G .: emptyPipe
