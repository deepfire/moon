{-# LANGUAGE UndecidableInstances       #-}
module Dom.Pipe.Constr (module Dom.Pipe.Constr) where

import           Generics.SOP                       (All)
import           Type.Reflection                    (Typeable)

import Dom.CTag
import Dom.VTag


--------------------------------------------------------------------------------
-- * IsCTagV
--
type IsCTagVConstr ct =
  ( Typeable ct
  , Typeable (CTagVC ct)
  , Typeable (CTagVV ct)
  , ct ~ CTagV (CTagVC ct) (CTagVV ct)
  )

class    (IsCTagVConstr ct) => IsCTagV (ct :: *)
instance (IsCTagVConstr ct) => IsCTagV (ct :: *)

--------------------------------------------------------------------------------
-- * Pipe constraints
--
type ArgConstr (a :: *)
  = ( IsCTagV a
    , ReifyCTag (CTagVC a), ReifyVTag (CTagVV a)
    )

type PipeConstr (as :: [*]) (o :: *)
  = ( ArgConstr o
    , All Typeable as -- why do we need this, when we have IsCTagV?
    , All IsCTagV as
    )
