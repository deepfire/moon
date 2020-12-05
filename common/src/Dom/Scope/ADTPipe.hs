{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Dom.Scope.ADTPipe (module Dom.Scope.ADTPipe) where

import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.Some                as SOP
import qualified Type.Reflection                  as Refl

import Basis

import Dom.CTag
import Dom.Ground
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Scope.SomePipe
import Dom.VTag

import Ground.Table


--------------------------------------------------------------------------------
-- * Scope of pipes, that offer projections for a GroundData-constrained ADT.
--
dataProjScope
  :: forall u.
  ( Typeable u, SOP.HasDatatypeInfo u, SOP.Generic u
  , ReifyVTag u
  -- XXX: why the combinatory explosion?
  , All2 (And Typeable                    Top)  (Code u)
  , All2 (And ReifyVTag                   Top)  (Code u)
  , All2 (And Typeable (And ReifyVTag Top)) (Code u)
  )
  => Proxy u -> Scope Point (SomePipe Dynamic)
dataProjScope  p = dataProjScope' p $ dataProjPipes (T mempty) (Proxy @Top) p

dataProjScopeG
  :: forall u. (GroundData u)
  => Proxy u -> Scope Point (SomePipe Dynamic)
dataProjScopeG p = dataProjScope' p $ dataProjPipes (G mempty) (Proxy @Ground) p

dataProjScope'
  :: forall u. Typeable u
  => Proxy u -> [SomePipe Dynamic] -> SomePipeScope Dynamic
dataProjScope' _p ps = pipeScope name ps
  where name  = Name $ pack $ show (Refl.typeRepTyCon (typeRep @u))

-- XXX: all dataProjPipes pipes are ungrounded on output.
dataProjPipes
  :: forall c c' u
  . ( Typeable c, Typeable u
    , SOP.HasTypeData c u, SOP.Generic u
    , ReifyVTag u
    , All2 (And Typeable (And ReifyVTag c)) (Code u)
    , All2 c' (Code u)
    , c' ~ (And ReifyVTag c)
    , c u)
  => (forall (cas :: [*]) (o :: *)
      .  PipeConstr c cas o
      => Pipe c (cas :: [*]) (o :: *) Dynamic
      -> SomePipe Dynamic)
  -> Proxy c -> Proxy u -> [SomePipe Dynamic]
dataProjPipes ctor _c u =
  let d :: SOP.Data SOP.Fun c' u
      d = SOP.typeData (Proxy @c') u
      fieldPipe
        :: ()
        => SOP.Data  SOP.Fun c' u
        -> SOP.Ctor  SOP.Fun c' u
        -> SOP.Field SOP.Fun c' u
        -> SomePipe Dynamic
      fieldPipe _d _c f =
        case SOP.fAccess f of
          SOP.SomeAccessors (SOP.Accessors getter _ :: SOP.Accessors u c' a) ->
            ctor $
            (pipe1
             (Name $ SOP.fName f)
             CVPoint
             CVPoint -- XXX: Kind can be non-Point!
             (pure . Right . getter))
  in [ fieldPipe d ct f
     | ct <- SOP.dCtors  d
     , f  <- SOP.cFields ct]
