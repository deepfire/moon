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
    Collect
  , Forget
  , FieldDesc(..)
  , collect
  , mapSOP
    -- * Re-exports
  , Generic
  , ConstructorInfo(..)
  , DatatypeInfo(..)
  , FieldInfo(..)
  , Proxy(..)
  )
where

import Data.Kind
import Data.SOP.NP
import Generics.SOP
import qualified Generics.SOP as SOP
import Generics.SOP.Traversal


-- | Generic data structure mapper used by 'mapSOP'.
type Collect f u xss xs c a
  =  Proxy c
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> FieldInfo a
  -> (u -> a)
  -> f c a

-- | Capture all information available about a single field.
data FieldDesc u xss (c :: Type -> Constraint) a = forall xs. c a => FieldDesc
  { fProxy           :: Proxy c
  , fDatatypeInfo    :: DatatypeInfo xss
  , fConstructorInfo :: ConstructorInfo xs
  , fFieldInfo       :: FieldInfo a
  , fGetter          :: u -> a
  }

-- | Collapse typed evidence of structure of a single field.
type Forget u xss c a g
  =  FieldDesc u xss c a -> g

-- | Collect collapsed information about a data type,
--   as a list of per-constructor lists of per-field extractions,
--   by mapping over typed evidence of structure, as represented
--   by FieldDesc.
collect
  :: forall g u xss c
  . ( HasDatatypeInfo u
    , Code u ~ xss
    , All2 c xss)
  => (forall a.  Forget u xss c a g)
  -> [[g]]
collect forget =
  mapSOP FieldDesc forget (Proxy @u) (Proxy @(FieldDesc u xss c))

-- | A generalised version of 'collect', which presents a choice
--   of intermediate type, instead of settling on 'FieldDesc'.
mapSOP
  :: forall g (f :: (Type -> Constraint) -> Type -> Type) u xss (c :: Type -> Constraint).
    ( Code u ~ xss
    , HasDatatypeInfo u
    , All2 c xss)
  => (forall a xs. c a => Collect f u xss xs c a)
  -> (forall a.    c a => f c a -> g)
  -> Proxy u
  -> Proxy (f c)
  -> [[g]]
mapSOP field forget pU@(datatypeInfo -> dti) _pFC =
  case dti of
    SOP.ADT _moduleName _typeName _cInfos -> do
      let pop :: POP (f c) xss = mapSum' field pU dti
          u   :: POP (K g) xss = cliftA_POP (Proxy @c) (K . forget) pop
      hcollapse u
    SOP.Newtype _moduleName _typeName cInfo -> do
      let nCInfos = cInfo :* Nil
          np  :: NP  (f c) _   = mapProduct field pU dti
                                           (hd nCInfos)
                                           (hd gtraversals)
          u   :: NP  (K g) _   = cliftA_NP (Proxy @c) (K . forget) np
      [hcollapse u]

mapSum'
  :: forall f u xss (c :: Type -> Constraint)
  . ( Generic u
    , Code u ~ xss
    , All2 c xss)
  => (forall a xs. (All c xs, c a) => Collect f u xss xs c a)
  -> Proxy u
  -> DatatypeInfo xss
  -> POP (f c) xss
mapSum' f pU dti@(SOP.ADT _ _ cinfos) =
  POP $ cliftA2_NP (Proxy @(All c))
        (mapProduct f pU dti)
        (cinfos      :: NP ConstructorInfo               xss)
        (gtraversals :: NP (NP (GTraversal (->) (->) u)) xss)
mapSum' _ _ _ = error "Non-ADTs not supported."

mapProduct
  :: forall f u xss xs (c :: Type -> Constraint)
  . All c xs
  => (forall a. (All c xs, c a) => Collect f u xss xs c a)
  -> Proxy u
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> NP (GTraversal (->) (->) u) xs
  -> NP (f c) xs
mapProduct f pU dti           consi@(Record _ finfos)  travs = mapFields f pU dti consi travs finfos
mapProduct f pU dti           consi@(Constructor ctor) travs = mapFields f pU dti consi travs (hpure (FieldInfo $ "un"<>ctor))
mapProduct _ _ (SOP.ADT _ ty _) _ _                          = error $ "Infix ADTs not supported: type "<>ty
mapProduct _ _ (SOP.Newtype _ ty _) _ _                      = error $ "Infix newtypes not supported: type "<>ty

mapFields
  :: forall f u xss xs (c :: Type -> Constraint)
  . All c xs
  => (forall a. c a => Collect f u xss xs c a)
  -> Proxy u
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> NP (GTraversal (->) (->) u) xs
  -> NP (FieldInfo) xs
  -> NP (f c) xs
mapFields f _pU dtinfo cinfo traversals finfos =
  hcliftA2 (Proxy @c)
  (mapField f dtinfo cinfo)
  finfos
  traversals

mapField
  :: forall f u xss xs (c :: Type -> Constraint) a
  . c a
  => Collect f u xss xs c a
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> FieldInfo a
  -> GTraversal (->) (->) u a
  -> f c a
mapField f dtinfo cinfo finfo trav =
  f (Proxy @c) dtinfo cinfo finfo (gtravget trav :: u -> a)
