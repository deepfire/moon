{-# LANGUAGE UndecidableInstances       #-}
module Dom.Pipe.Constr (module Dom.Pipe.Constr) where

import           Data.Kind                          (Constraint)
import           Generics.SOP                       (All, Top)
import           Type.Reflection                    (Typeable)

import qualified Unsafe.Coerce                    as Unsafe

import Dom.CTag
import Dom.VTag


--------------------------------------------------------------------------------
-- * IsCTagV
--
type IsCTagVConstr ct =
  ( Typeable ct
  , Typeable (CTagVC ct)
  , Typeable (CTagVV ct)
  , Top ct
  , ct ~ CTagV (CTagVC ct) (CTagVV ct)
  )

class    (IsCTagVConstr ct) => IsCTagV (ct :: *)
instance (IsCTagVConstr ct) => IsCTagV (ct :: *)

--------------------------------------------------------------------------------
-- * Pipe constraints
--
type ArgConstr (c :: * -> Constraint) (a :: *)
  = ( Typeable c
    , IsCTagV a
    , c (CTagVV a)
    , ReifyCTag (CTagVC a), ReifyVTag (CTagVV a)
    )

type PipeConstr (c :: * -> Constraint) (as :: [*]) (o :: *)
  = ( ArgConstr c o
    , All Typeable as -- why do we need this, when we have IsCTagV?
    , All IsCTagV as
    -- , All Top as
    )
