{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Moon.Reflection
  ( -- *
    typeVariants
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Control.Monad.Class.MonadST
import           Data.Dynamic
import           Data.Foldable                      (asum)
import qualified Data.Map                         as Map
import           Data.Map                           (Map)
import           Data.Maybe                         (fromMaybe)
import           Data.String                        (IsString)
import           Data.Text                          (Text, pack, unpack)
import           Data.Typeable
import qualified Data.Set                         as S
import qualified Data.Kind                        as Kind
import           Data.Proxy                         (Proxy(..))
import           GHC.Generics                hiding ((:.:))
import qualified GHC.Generics                     as GHC
import           Options.Applicative
import           Type.Reflection
import qualified Unsafe.Coerce                    as Unsafe

import qualified Moon.Face                        as Face
import           Moon.Face                   hiding (Type)
import           Moon.Face.Ground

import           Generics.SOP                     as SOP
import           Generics.SOP.Traversal           as SOP

-- typeVariants :: Type -> Maybe [Variant]
-- typeVariants (Type _ _ str) = someTypeRepVariants str

someTypeRepVariants :: SomeTypeRep -> Maybe [Variant]
someTypeRepVariants str =
  withGroundType str strVariants
  where strVariants :: Dict GroundContext -> [Variant]
        strVariants (Dict (p :: Proxy a)) =
          undefined


data Accessor = forall a b. (GroundContext a, GroundContext b) => Accessor (a -> b)

recover
  :: forall a (c :: Kind.Type -> Kind.Constraint) xss.
    ( Code a ~ xss, HasDatatypeInfo a
    , All2 c xss)
  => Proxy c
  -> Proxy a
  -> [Variant]
recover pC pA = let dti = datatypeInfo pA :: DatatypeInfo xss
  in
  case dti of
    ADT _moduleName typeName cInfos -> do
      let pop        ::  POP I xss     = recover' pC pA dti
          cts        :: [SOP I xss]    = SOP.apInjs_POP pop
      case SOP.sList :: SOP.SList xss of
        SOP.SCons -> (SOP.to <$>) <$> mrsop
    Newtype _moduleName typeName cInfo -> do
      let nCInfos -- ~:: NP ((,) Int :.: ConstructorInfo) '[ '[x]]
            = cInfo :* Nil
          sop        :: SOP I xss =
            SOP.SOP $ SOP.Z $
            recoverCtor pC pA dti
            (SOP.hd nCInfos)
            (SOP.hd ((gtraversals -- ~:: NP (NP (GTraversal (->) (->) s)) '[ '[x]]
                        )))
          mdsop ::     SOP I xss = sop
      (SOP.to <$>) <$> mdsop

recover'
  :: forall a (c :: Kind.Type -> Kind.Constraint) xss.
    ( SOP.Generic a, Code a ~ xss
    , All2 c xss)
  => Proxy c
  -> Proxy a
  -> DatatypeInfo xss
  -> POP I xss
recover' pC pTA dti@(ADT _ name cs) =
  POP $ SOP.hcliftA2 (Proxy @(All c))
        (recoverCtor pC pTA dti)
        cs
        (gtraversals :: NP (NP (GTraversal (->) (->) a)) xss)
recover' _ _ _ = error "Non-ADTs not supported."

-- * 1. Extract the constructor's product of field names
--   2. Feed that to the field-name->action interpreter
recoverCtor
  :: forall a (c :: Kind.Type -> Kind.Constraint) xss xs.
    ( Code a ~ xss
    , All c xs)
  => Proxy c
  -> Proxy a
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> NP (GTraversal (->) (->) a) xs
  -> NP I xs
recoverCtor pC pTA dti (consi@(Record _ finfos)) travs = recoverFields pC pTA dti consi travs finfos
recoverCtor pC pTA dti (consi@Constructor{})     travs = recoverFields pC pTA dti consi travs (SOP.hpure (FieldInfo ""))
recoverCtor _ _ (ADT _ name _) _ _ =
  error $ "Infix ADTs not supported: type "<>name

-- * Key part:  NP (K Text) xs -> NP m xs
--   convert a product of field names to a product of monadic actions yielding 'a'
recoverFields
  :: forall (c :: Kind.Type -> Kind.Constraint) u xss xs.
    ( Code u ~ xss
    , All c xs
    , SListI xs)
  => Proxy c
  -> Proxy u
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> NP (GTraversal (->) (->) u) xs
  -> NP (FieldInfo) xs
  -> NP I xs
recoverFields pC _pTU dtinfo cinfo traversals finfos =
  hcliftA2 pC
  (recoverField dtinfo cinfo)
  finfos
  traversals
  where
    recoverField :: forall a. (c a)
                 => DatatypeInfo xss
                 -> ConstructorInfo xs
                 -> FieldInfo a
                 -> GTraversal (->) (->) u a
                 -> I a
    recoverField dtinfo cinfo finfo trav =
      undefined (Proxy @c) (Proxy @(u, a))
      dtinfo cinfo finfo (gtravget trav :: u -> a)

