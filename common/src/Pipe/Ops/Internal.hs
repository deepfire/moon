{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Pipe.Ops.Internal
Description : Guts of the Pipe guts.
Copyright   : (c) Kosyrev Serge, 2019
License     : GPL-3

Private structure of 'Pipe':  types and patterns.
-}

module Pipe.Ops.Internal
  ( typeRepNull
  , IOA(..)
  , ioaTyInvalidity
  , ioaTyConsInvalidity
  , ioaTySingletonInvalidity
  , ioaTyNilInvalidity
  , Pipe(.., P)
  -- GHC bug: we shouldn't need to list them explicitly.
  , pDesc_, pName, pStruct, pPipeRep, pPipe, pArgStys, pOutSty, pArgs, pOut
  , SomeTypeRep(.., IOATyCons, IOATyNil)
  -- GHC bug: we shouldn't need to list them explicitly.
  , ioaCon, listCon --, constrRep
  , typeACon, tagARep, aRep
  , restRep
  , typeOCon, tagORep, oRep
  --
  , module Basis
  , module Type
  , module SomeType
  , module SomeValue
  , module Pipe.Types
  )
where

import Type.Reflection ( pattern App
                       , pattern Con
                       , TyCon
                       , (:~~:)(..)
                       , eqTypeRep
                       , someTypeRepTyCon
                       , splitApps
                       , typeRepTyCon
                       )

import Basis
import Pipe.Types
import Type
import SomeType
import SomeValue

-- * Guts of the pipe guts.
--
data IOA (c :: * -> Constraint) (as :: [*]) (o :: *) where
  IOA :: PipeConstr c as o
      => PipeFunTy as o
      -> Proxy c
      -> Proxy as
      -> Proxy o
      -> IOA c as o

pattern IOATyCons
  :: TyCon -> TyCon -- -> TypeRep c
  -> TyCon -> TypeRep ka -> TypeRep a
  -> TypeRep rest
  -> TyCon -> TypeRep ko -> TypeRep o
  -> SomeTypeRep
pattern IOATyCons
  { ioaCon, listCon --, constrRep
  , typeACon, tagARep, aRep
  , restRep
  , typeOCon, tagORep, oRep
  }
  <- SomeTypeRep (App
                  (App (App (Con ioaCon) (Con _constrRep))
                       (App (App (Con listCon)
                                 (App (App (Con typeACon) tagARep)
                                      aRep))
                            restRep))
                  (App (App (Con typeOCon) tagORep)
                       oRep))

pattern IOATyNil
  :: TyCon -> TyCon -> TyCon -> TypeRep ko -> TypeRep o -> SomeTypeRep
pattern IOATyNil ioaCon nilCon typeOCon tagORep oRep
  <- SomeTypeRep (App
                  (App (App (Con ioaCon) (Con _cstr))
                       (Con nilCon))
                  (App (App (Con typeOCon) tagORep)
                       oRep))

typeRepNull
  :: forall k (a :: [k]) (b :: [k])
   . (Typeable k, b ~ '[])
  => TypeRep a
  -> Maybe (a :~~: b)
typeRepNull rep = rep `eqTypeRep` typeRep @('[] :: [k])

consTyCon, ioaTyCon, nilTyCon, typeTyCon :: TyCon
consTyCon = typeRepTyCon (typeRep @(() : '[]))
nilTyCon  = someTypeRepTyCon (head $ tail $ snd $ splitApps $ typeRep @(() : '[]))
typeTyCon = typeRepTyCon (typeRep @Type)
ioaTyCon = typeRepTyCon (typeRep @IOA)

ioaTyInvalidity :: SomeTypeRep -> Maybe Text
ioaTyInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= ioaTyCon  = Just "not an IOA"
  | lcon /= nilTyCon &&
    lcon /= consTyCon = Just ("arglist type not a list: " <> pack (show lcon))
  | ocon /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyInvalidity _     = Just "no match with an IOA"

ioaTyConsInvalidity :: SomeTypeRep -> Maybe Text
ioaTyConsInvalidity IOATyCons{ioaCon=ioa, listCon=list, typeACon=tyA, typeOCon=tyO}
  |   ioa /= ioaTyCon  = Just "not an IOA"
  |  list /= consTyCon = Just "arglist type not a nonempty list"
  | tyA   /= typeTyCon = Just "first arg not a Type"
  | tyO   /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyConsInvalidity _ = Just "no match with IOATyCons"

ioaTySingletonInvalidity :: SomeTypeRep -> Maybe Text
ioaTySingletonInvalidity rep@IOATyCons{}
  | Just e <- ioaTyConsInvalidity rep = Just e
  | otherwise = case rep of
      IOATyCons{restRep=Con{}} -> Nothing
      _ -> Just "arglist type not a singleton list"
ioaTySingletonInvalidity _ = Just "arglist type not a singleton list"

ioaTyNilInvalidity :: SomeTypeRep -> Maybe Text
ioaTyNilInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= ioaTyCon  = Just "not an IOA"
  | lcon /= nilTyCon  = Just "arglist type not an empty list"
  | ocon /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyNilInvalidity _  = Just "no match with IOATyNil"

pattern P
  :: Desc c as o -> Name Pipe -> Struct -> SomeTypeRep -> p
  -> [SomeType]   -> SomeType
  -> NP TypePair as -> TypePair o
  -> Pipe c as o p
pattern P { pDesc_, pName, pStruct, pPipeRep, pPipe, pArgStys, pOutSty, pArgs, pOut }
  <- Pipe pDesc_@(Desc pName (Sig (fmap unI -> pArgStys) (I pOutSty)) pStruct
                  pPipeRep pArgs pOut)
          pPipe
