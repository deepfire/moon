{-# LANGUAGE UndecidableInstances       #-}
module Dom.Pipe.Constr (module Dom.Pipe.Constr) where

import           Data.Kind                          (Constraint)
import           Generics.SOP                       (All, Top)
import           Type.Reflection                    (Typeable)

import Dom.CTag
import Dom.VTag


--------------------------------------------------------------------------------
-- * IsTypes
--
type IsTypesConstr ct =
  ( Typeable ct
  , Typeable (TypesC ct)
  , Typeable (TypesV ct)
  , Top ct
  , ct ~ Types (TypesC ct) (TypesV ct)
  )

class    (IsTypesConstr ct) => IsTypes (ct :: *)
instance (IsTypesConstr ct) => IsTypes (ct :: *)

--------------------------------------------------------------------------------
-- * Pipe constraints
--
type ArgConstr (c :: * -> Constraint) (ct :: *)
  = ( IsTypes ct
    , Typeable c
    , ReifyCTag (TypesC ct)
    , ReifyVTag (TypesV ct)
    , c (TypesV ct)
    )

type PipeConstr (c :: * -> Constraint) (cas :: [*]) (o :: *)
  = ( All IsTypes cas, ArgConstr c o
    , All Typeable cas -- why do we need this, when we have IsTypes?
    )
