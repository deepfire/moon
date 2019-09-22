{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Generics.SOP.Mapping
  ( -- *
    CollectADT
  , CollectCtor
  , CollectField
  , ADTDesc(..)
  , CtorDesc(..)
  , FieldDesc(..)
  , ForgetADT
  , ForgetCtor
  , ForgetField
  , collect
  , mapSOP
    -- * Re-exports
  , Generic
  , Const(..)
  , ConstructorInfo(..)
  , DatatypeInfo(..)
  , FieldInfo(..)
  , NP(..)
  , Proxy(..)
  )
where

import Data.Functor.Const (Const(..))
import Data.Kind
import Data.SOP.NP
import Generics.SOP
import qualified Generics.SOP as SOP
import Generics.SOP.Traversal


-- | Generic ADT mappers used by 'mapSOP',
--   that extract typed structure information.
type CollectADT   (a :: (* -> Constraint) -> [[*]] -> *)
                  (r :: (* -> Constraint) -> [*] -> *)
                  (f :: (* -> Constraint) -> * -> *)
                   u
                   c
                  (xss :: [[*]])
  =  Proxy c
  -> DatatypeInfo xss
  -> NP (r c) xss
  -> a c xss

type CollectCtor  (r :: (* -> Constraint) -> [*] -> *)
                  (f :: (* -> Constraint) -> * -> *)
                   u
                   c
                  (xs :: [*])
  =  Proxy c
  -> Proxy u
  -> ConstructorInfo xs
  -> NP (f c) xs
  -> r c xs

type CollectField (f :: (* -> Constraint) -> * -> *)
                   u
                   c
                  (x :: *)
  =  Proxy c
  -> FieldInfo x
  -> (u -> x)
  -> f c x

-- | Typed intermediate representation of structure.
data ADTDesc u (c :: * -> Constraint) xss = All2 c xss => ADTDesc
  { aProxy     :: Proxy c
  , aInfo      :: DatatypeInfo xss
  , aCtors     :: NP (CtorDesc c) xss
  }

data CtorDesc  (c :: * -> Constraint) xs = forall u. All c xs => CtorDesc
  { cProxy     :: Proxy c
  , cProxyU    :: Proxy u
  , cInfo      :: ConstructorInfo xs
  , cFields    :: NP (FieldDesc u c) xs
  }

data FieldDesc u (c :: * -> Constraint) x = c x => FieldDesc
  { fProxy     :: Proxy c
  , fInfo      :: FieldInfo x
  , fGetter    :: u -> x
  }

-- | Collapse typed evidence.
type ForgetADT   u c (xss :: [[*]]) fa
  =  ADTDesc     u c  xss ->        fa

type ForgetCtor  u c (xs :: [*])    fr
  =  CtorDesc      c  xs  ->        fr

type ForgetField u c (x :: *)       ff
  =  FieldDesc   u c  x   ->        ff

-- | Collect collapsed information about a data type,
--   as a list of per-constructor lists of per-field extractions,
--   by mapping over typed evidence of structure, as represented
--   by FieldDesc.
collect
  :: forall u c xss fa fr ff
  . ( HasDatatypeInfo u
    , Code u ~ xss
    , All2 c xss)
  => Proxy c -> Proxy u -> Proxy fr -> Proxy ff
  -> (          All2 c xss => ForgetADT   u c xss fa)
  -> (forall xs. All c xs  => ForgetCtor  u c xs  fr)
  -> (forall x.      c x   => ForgetField u c x   ff)
  -> fa
collect c u fr ff fADT fCtor fField =
  mapSOP c u (Proxy @(FieldDesc u)) fr ff
         ADTDesc fADT
         CtorDesc fCtor
         FieldDesc fField

-- | A generalised version of 'collect', which presents a choice
--   of intermediate type, instead of settling on 'FieldDesc'.
mapSOP
  :: forall
    (a :: (* -> Constraint) -> [[*]] -> *) (fa :: *)
    (r :: (* -> Constraint) -> [*] -> *)   (fr :: *)
    (f :: (* -> Constraint) -> * -> *)     (ff :: *)
    u xss
    (c :: * -> Constraint)
  . ( Code u ~ xss
    , HasDatatypeInfo u
    , All2 c xss)
  => Proxy c -> Proxy u -> Proxy f -> Proxy fr -> Proxy ff
  -> (          All2 c xss => CollectADT a r f u c xss)
  -> (          All2 c xss => ForgetADT        u c xss fa)
  -> (forall xs. All c xs  => Proxy c
                           -> Proxy u
                           -> ConstructorInfo xs
                           -> NP (f c) xs
                           -> r c xs
      -- CollectCtor  r f u c xs
     )
  -> (forall xs. All c xs  => ForgetCtor       u c xs  fr)
  -> (forall x.      c x   => CollectField   f u c x)
  -> (forall x.      c x   => ForgetField      u c x   ff)
  -> fa
mapSOP c u@(datatypeInfo -> dti) f _fr _ff
       adt   fadt
       ctor  fctor
       field ffield =
  case dti of
    SOP.ADT _moduleName _typeName _cInfos ->
      let typed :: a c xss
          typed = mapSum' c u f
                  adt ctor field dti
          fi :: c x => f c x -> ff
          fi = undefined
          -- apFCt :: All c xs => r c xs -> fr
          -- apFCt cts = fctor cts (hliftA (K . ffield) cts)
          -- fis :: All c xs => r c xs -> NP (K ff) xs -> rf
          -- fis = hliftA
          fFi :: NP (NP (K ff)) xss
          fFi = undefined --hliftA (hliftA ffield) typed
          fCt :: NP (K fr) xss
          fCt = undefined --hliftA fctor fFi
      in undefined
      -- let pop :: POP (f c) xss = mapSum' field pU dti
      --     u   :: POP (K g) xss = cliftA_POP (Proxy @c) (K . forget) pop
      -- hcollapse u
    -- SOP.Newtype _moduleName _typeName cInfo -> do
      -- let nCInfos = cInfo :* Nil
      --     np  :: NP  (f c) _   = mapProduct field pU dti
      --                                      (hd nCInfos)
      --                                      (hd gtraversals)
      --     u   :: NP  (K g) _   = cliftA_NP (Proxy @c) (K . forget) np
      -- [hcollapse u]

mapSum'
  :: forall (a :: (* -> Constraint) -> [[*]] -> *)
            (r :: (* -> Constraint) -> [*] -> *)
            (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint) xss
  . ( Generic u
    , Code u ~ xss
    , All2 c xss)
  => Proxy c -> Proxy u -> Proxy f
  -> (          All2 c xss => CollectADT a r f u c xss)
  -> (forall xs. All c xs  => CollectCtor  r f u c xs)
  -> (forall x.      c x   => CollectField   f u c x)
  -> DatatypeInfo xss
  -> a c xss
mapSum' c _u _f adt ctor field dti@(SOP.ADT _ _ cinfos) =
  adt c dti $
    cliftA2_NP (Proxy @(All c))
    (mapProduct c ctor field dti)
    (cinfos      :: NP ConstructorInfo               xss)
    (gtraversals :: NP (NP (GTraversal (->) (->) u)) xss)
mapSum' _ _ _ _ _ _ _ = error "Non-ADTs not supported."

mapProduct
  :: forall (r :: (* -> Constraint) -> [*] -> *)
            (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint)
            (xs :: [*]) (xss :: [[*]])
  . All c xs
  => Proxy c
  -> (           All c xs  => CollectCtor  r f u c xs)
  -> (forall x.      c x   => CollectField   f u c x)
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> NP (GTraversal (->) (->) u) xs
  -> r c xs
mapProduct c ct fi _ consi@(Record _ finfos)  travs = mapFields c ct fi consi finfos travs
mapProduct c ct fi _ consi@(Constructor ctor) travs = mapFields c ct fi consi (hpure (FieldInfo $ "un"<>ctor)) travs
mapProduct _ _ _ (SOP.ADT _ ty _) _ _     = error $ "Infix ADTs not supported: type "<>ty
mapProduct _ _ _ (SOP.Newtype _ ty _) _ _ = error $ "Infix newtypes not supported: type "<>ty

mapFields
  :: forall (r :: (* -> Constraint) -> [*] -> *)
            (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint) (xs :: [*])
  . All c xs
  => Proxy c
  -> (           All c xs  => CollectCtor  r f u c xs)
  -> (forall x.      c x   => CollectField   f u c x)
  -> ConstructorInfo xs
  -> NP (FieldInfo) xs
  -> NP (GTraversal (->) (->) u) xs
  -> r c xs
mapFields c ctor field cinfo finfos traversals =
  ctor c (Proxy @u) cinfo $
  hcliftA2 c
  (mapField c field)
  finfos
  traversals

mapField
  :: forall (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint) (x :: *)
  . c x
  => Proxy c
  -> CollectField f u c x
  -> FieldInfo x
  -> GTraversal (->) (->) u x
  -> f c x
mapField c field finfo trav =
         field c finfo (gtravget trav :: u -> x)
