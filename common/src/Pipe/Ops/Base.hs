module Pipe.Ops.Base
  ( runPipe
  , gen,  genG, gen'
  , link, linkG, link'
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)

import Pipe.Ops.Internal


-- * Running
--
runPipe :: SomePipe Dynamic -> Result SomeValue
runPipe T{tPipe=t@Pipe{pDesc, p}} = pure . Left $ "runPipe:  non-Ground pipe: " <> showPipe t
runPipe G{gPipe=  Pipe{pDesc, p}} = runPipe' pDesc p

runPipe'
  :: forall c (as :: [*]) o. PipeConstr c as o
  => Desc c as o
  -> Dynamic
  -> Result SomeValue
runPipe' pd@Desc{pdOut=Tags{tCTag, tVTag}} dyn =
  case fromDynamic dyn :: Maybe (IOA Ground '[] o) of
    Nothing -> pure . Left $ "Incomplete pipe: " <> showDesc pd
    Just (IOA io _c _as _o) ->
      (SomeValue tCTag . SomeValueKinded tVTag . mkValue tVTag tCTag <$>) <$> io


-- * Constructors
--
genG
  :: forall ct tt
  .  (ReifyCTag ct, ReifyVTag tt, Typeable (Repr ct tt), Typeable ct, Ground tt)
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> SomePipe Dynamic
genG n to pf = G mempty $ gen' n to pf

gen
  :: forall ct tt
  .  ( ReifyCTag ct
     , ReifyVTag tt
     , Typeable (Repr ct tt), Typeable ct, Typeable tt)
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> SomePipe Dynamic
gen n to pf = T mempty $ gen' n to pf

linkG
  :: forall cf tf ct tt
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable (Repr cf tf), Typeable (Repr ct tt)
    , Typeable cf, Typeable tf, Typeable ct
    , Ground tt)
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> SomePipe Dynamic
linkG n from to pf = G mempty $ link' n from to pf

link
  :: forall cf tf ct tt
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable (Repr cf tf), Typeable (Repr ct tt)
    , Typeable cf, Typeable tf, Typeable ct
    , Typeable tt)
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> SomePipe Dynamic
link n from to pf = T mempty $ link' n from to pf

gen'
  :: forall cf tf ct tt c
  .  ( cf ~ 'Point, tf ~ ()
     , ReifyCTag ct, ReifyVTag tt
     , Typeable (Repr ct tt), Typeable ct, Typeable tt, Typeable c
     , c tt)
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> Pipe c '[] (Types ct tt) Dynamic
gen' name typ@(typesTags -> Tags cTag vTag) mv
  -- TODO: validate types against the typerep/dynamic
                = Pipe desc dyn
  where ty      = someType typ
        desc    = Desc name sig struct (dynRep dyn) Nil (Tags cTag vTag)
        sig     = Sig [] (I ty)
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mv Proxy Proxy Proxy ::
                  IOA c '[] (Types ct tt)

link' ::
    forall cf tf ct tt c
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable (Repr cf tf), Typeable (Repr ct tt)
    , Typeable cf, Typeable tf, Typeable ct, Typeable tt, Typeable c
    , c tt)
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> Pipe c '[Types cf tf] (Types ct tt) Dynamic
link' name typf@(typesTags -> Tags cf tf) typt@(typesTags -> Tags ct tt) mf
                = Pipe desc dyn
  where desc    = Desc name sig struct (dynRep dyn) (Tags cf tf :* Nil) (Tags ct tt)
        sig     = Sig [I $ someType typf] (I $ someType typt)
        struct  = Struct G.empty
        -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)
        ---------
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mf Proxy Proxy Proxy ::
                  IOA c '[Types cf tf] (Types ct tt)

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
