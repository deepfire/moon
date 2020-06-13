{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module Pipe.Scope
  ( SomePipeScope
  , pipeScope
  , emptyPipeScope
  , dataProjScope
  , dataProjScopeG
  -- * Re-exports
  , Scope
  , scope
  , emptyScope
  , scopeNames
  , scopeEntries
  , lookupScope
  , updateScope
  , alterScope
  )
where

import qualified Type.Reflection                  as R

import Basis
import qualified Generics.SOP.Some as SOP
import Namespace
import Pipe.Ops
import Pipe.Types


--------------------------------------------------------------------------------
-- TODO: make this a generic operation?
dataProjScope
  :: forall u.
  ( Typeable u, SOP.HasDatatypeInfo u, SOP.Generic u
  , All2 (SOP.And Typeable Top) (SOP.Code u)
  )
  => Proxy u -> Scope Point (SomePipe Dynamic)
dataProjScope  p = dataProjScope' p $ dataProjPipes T (Proxy @Top) p

dataProjScopeG
  :: forall u. (GroundData u)
  => Proxy u -> Scope Point (SomePipe Dynamic)
dataProjScopeG p = dataProjScope' p $ dataProjPipes G (Proxy @Ground) p

dataProjScope'
  :: forall u. Typeable u
  => Proxy u -> [SomePipe Dynamic] -> SomePipeScope Dynamic
dataProjScope' _p ps = pipeScope name ps
  where name  = Name $ pack $ show (R.typeRepTyCon (typeRep @u))


--------------------------------------------------------------------------------
emptyPipeScope :: Name Scope -> SomePipeScope p
emptyPipeScope = Namespace.emptyScope . coerceName

pipeScope :: Name Scope -> [SomePipe p] -> SomePipeScope p
pipeScope name pipes = scope (coerceName name) $
  zip (coerceName . somePipeName <$> pipes) pipes

dataProjPipes
  :: forall c u
  . ( Typeable c, Typeable u
    , SOP.HasTypeData c u, SOP.Generic u
    , All2 (SOP.And Typeable c) (SOP.Code u)
    , c u)
  => (forall (kas :: [*]) (o :: *)
      .  PipeConstr c kas o
      => Pipe c (kas :: [*]) (o :: *) Dynamic
      -> SomePipe Dynamic)
  -> Proxy c -> Proxy u -> [SomePipe Dynamic]
dataProjPipes ctor c u =
  let d :: SOP.Data SOP.Fun c u
      d = SOP.typeData c u
      fieldPipe
        :: SOP.Data  SOP.Fun c u
        -> SOP.Ctor  SOP.Fun c u
        -> SOP.Field SOP.Fun c u
        -> SomePipe Dynamic
      fieldPipe _d _c f =
        case SOP.fAccess f of
          SOP.SomeAccessors
            (SOP.Accessors getter _ :: SOP.Accessors u c a) ->
            ctor $
            link' (Name $ SOP.fName f)
            TPoint'
            TPoint' -- XXX: Kind can be non-Point!
            (pure . Right . getter)
  in [ fieldPipe d ct f
     | ct <- SOP.dCtors  d
     , f  <- SOP.cFields ct]
