{-|
Module      : Pipe.Ops.Internal
Description : Guts of the Pipe guts.
Copyright   : (c) Kosyrev Serge, 2019
License     : GPL-3

Private structure of 'Pipe':  types and patterns.
-}

module Pipe.Ops.Internal
  ( IOA(..)
  , IOA'(..)
  , pattern App4
  , pattern P
  )
where

import Type.Reflection ( pattern App
                       , pattern Con
                       , TyCon)
import Pipe.Types
import Type

-- * Guts of the pipe guts.
--
data IOA (c :: * -> Constraint) (as :: [*]) (o :: *) where
  IOA :: PipeConstr c as o
      => PipeFunTy as o
      -> Proxy c
      -> Proxy as
      -> Proxy o
      -> IOA c as o

data IOA' c ka a kb b where
  IOA' :: (Typeable ka, Typeable a, ReifyTag kb, Typeable kb, Typeable b, c b)
      => (Repr ka a -> Result (Repr kb b))
      -> IOA' c ka a                 kb b

pattern App4
  :: TyCon -> TypeRep ka -> TypeRep a -> TypeRep kb -> TypeRep b -> SomeTypeRep
pattern App4 con ka a kb b <- SomeTypeRep (App (App (App (App (App (Con con) _) ka) a) kb) b)

pattern P
  :: Name Pipe -> Struct -> SomeTypeRep -> p -> [SomeType] -> SomeType
  -> Pipe c as o p
pattern P  n st rep p as o <- Pipe (Desc n (Sig as o) st rep _ _) p
