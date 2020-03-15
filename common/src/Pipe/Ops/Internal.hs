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
  , pattern IOATyCons
  , pattern IOATyNil
  , pattern P
  , ioaTyInvalidity
  , ioaTyConsInvalidity
  , ioaTyNilInvalidity
  )
where

import Type.Reflection ( pattern App
                       , pattern Con
                       , TyCon
                       , typeRepTyCon
                       )
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

pattern IOATyCons
  :: TyCon -> TyCon
  -> TyCon -> TypeRep ka -> TypeRep a
  -> TyCon -> TypeRep ko -> TypeRep o
  -> SomeTypeRep
pattern IOATyCons con lcon acon ka a ocon ko o
  <- SomeTypeRep (App
                  (App (App (Con con) (Con _cstr))
                       (App (App (Con lcon)
                                 (App (App (Con acon) ka)
                                      a))
                            _rest))
                  (App (App (Con ocon) ko)
                       o))

pattern IOATyNil
  :: TyCon -> TyCon -> TyCon -> TypeRep ko -> TypeRep o -> SomeTypeRep
pattern IOATyNil con nilcon ocon ko o
  <- SomeTypeRep (App
                  (App (App (Con con) (Con _cstr))
                       (Con nilcon))
                  (App (App (Con ocon) ko)
                       o))

ioaTyInvalidity :: SomeTypeRep -> Maybe Text
ioaTyInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= typeRepTyCon (typeRep @IOA) = Just "not an IOA"
  | lcon /= typeRepTyCon (typeRep @([])) &&
    lcon /= typeRepTyCon (typeRep @(() : '[])) = Just "arglist type not a list"
  | ocon /= typeRepTyCon (typeRep @Type) = Just "output not a Type"
  | otherwise = Nothing
ioaTyInvalidity _ = Just "no match with an IOA"

ioaTyConsInvalidity :: SomeTypeRep -> Maybe Text
ioaTyConsInvalidity (IOATyCons con lcon acon _ka _a ocon _ko _o)
  |  con /= typeRepTyCon (typeRep @IOA) = Just "not an IOA"
  | lcon /= typeRepTyCon (typeRep @(() : '[])) = Just "arglist type not a nonempty list"
  | acon /= typeRepTyCon (typeRep @Type) = Just "first arg not a Type"
  | ocon /= typeRepTyCon (typeRep @Type) = Just "output not a Type"
  | otherwise = Nothing
ioaTyConsInvalidity _ = Just "no match with IOATyCons"

ioaTyNilInvalidity :: SomeTypeRep -> Maybe Text
ioaTyNilInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= typeRepTyCon (typeRep @IOA) = Just "not an IOA"
  | lcon /= typeRepTyCon (typeRep @([])) = Just "arglist type not an empty list"
  | ocon /= typeRepTyCon (typeRep @Type) = Just "output not a Type"
  | otherwise = Nothing
ioaTyNilInvalidity _ = Just "no match with IOATyNil"

pattern P
  :: Name Pipe -> Struct -> SomeTypeRep -> p -> [SomeType] -> SomeType
  -> Pipe c as o p
pattern P  n st rep p as o <- Pipe (Desc n (Sig as o) st rep _ _) p
