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

module Generics.SOP.Some
  ( -- *
    Data(..)
  , Ctor(..)
  , Field(..)
  , Form(..)
  , stillData
  , SomeAccessors(..)
  , Accessors(..)
    -- * Re-exports
  , Generic
  , Proxy(..)
  )
where

import Codec.Serialise
import Data.Kind
import Data.Text
import qualified GHC.Generics as GHC
import Generics.SOP
import qualified Generics.SOP.Mapping as SOP
import Text.Read
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)

import Data.Orphanage
import Generics.SOP.Mapping hiding (cFields)


data Form = Fun | Still

data Data  (p :: Form) (f :: (Type -> Constraint) -> Type) (c :: Type -> Constraint) = Data
  { moduleName :: !Text
  , typeName   :: !Text
  , dCtors     :: ![Ctor p f c]
  }

data Ctor  (p :: Form) (f :: (Type -> Constraint) -> Type) (c :: Type -> Constraint) = Ctor
  { cName      :: !Text
  , cFields    :: ![Field p f c]
  }

data family Const2 (f :: k1 -> k3) (x :: k1) (y :: k2)

data instance Const2 f x y where
  Const2 :: { unConst2 :: f x } -> Const2 f x y

mkCtor
  :: forall (u :: *) c xs. (All c xs)
  => CollectCtor
     (Const2 (Ctor Fun (Field Fun SomeAccessors)))
     (Const2 (Field Fun SomeAccessors))
     u c xs
mkCtor _c (Constructor name) tfs =
  Const2 $ Ctor (pack name)
                (hcollapse
                 (hliftA2 (K . forgetAccessors)
                          (undefined :: NP (FieldDesc u c) xs)
                          tfs))
  

data Field (p :: Form) (f :: (Type -> Constraint) -> Type) (c :: Type -> Constraint) = Field
  { fField     :: !(f c)
  , fName      :: !Text
  , fParentRep :: !SomeTypeRep
  , fRep       :: !SomeTypeRep
  , fAccess    :: !(Formed p c)
  }

forgetAccessors
  :: forall u c x. (Typeable u, Typeable x, c x)
  => ForgetField (Field Fun SomeAccessors c) (Accessors u) u c x
forgetAccessors (FieldDesc c (FieldInfo name) _) accessors =
  Field (SomeAccessors accessors)
        (pack name)
        (someTypeRep $ Proxy @u)
        (someTypeRep $ Proxy @x)
        (SomeAccessors accessors)

data SomeAccessors (c :: Type -> Constraint) = forall u a. c a => SomeAccessors
  { access      :: !(Accessors u c a)
  }

data Accessors u (c :: Type -> Constraint) a = c a => Accessors
  { getter      :: u -> a
  , setter      :: a -> u -> u
  }

mkAccessors
  :: forall u c x. c x
  => CollectField (Accessors u) u c x
mkAccessors _c (FieldInfo _name) get = Accessors get undefined

type family Formed f c :: Type where
  Formed Fun   c = SomeAccessors c
  Formed Still c = ()

stillData :: Data Fun c f -> Data Still c f
stillData d = d { dCtors = stillCtor <$> dCtors d }
  where stillCtor  c = c { cFields = stillField <$> cFields c }
        stillField f = f { fAccess = () }

deriving instance   Eq          (f c)  => Eq          (Data  Still f c)
deriving instance   GHC.Generic (f c)  => GHC.Generic (Data  Still f c)
deriving instance   Ord         (f c)  => Ord         (Data  Still f c)
deriving instance ( Typeable     f
                  , Typeable       c
                  , Read        (f c)) => Read        (Data  Still f c)
deriving instance ( GHC.Generic (f c)
                  , Serialise   (f c)) => Serialise   (Data  Still f c)
deriving instance   Show        (f c)  => Show        (Data  Still f c)

deriving instance   Eq          (f c)  => Eq          (Ctor  Still f c)
deriving instance   GHC.Generic (f c)  => GHC.Generic (Ctor  Still f c)
deriving instance   Ord         (f c)  => Ord         (Ctor  Still f c)
deriving instance ( Typeable     f
                  , Typeable       c
                  , Read        (f c)) => Read        (Ctor  Still f c)
deriving instance ( GHC.Generic (f c)
                  , Serialise   (f c)) => Serialise   (Ctor  Still f c)
deriving instance   Show        (f c)  => Show        (Ctor  Still f c)

deriving instance   Eq          (f c)  => Eq          (Field Still f c)
deriving instance   GHC.Generic (f c)  => GHC.Generic (Field Still f c)
deriving instance   Ord         (f c)  => Ord         (Field Still f c)
instance          ( Typeable f
                  , Typeable c)        => Read        (Field Still f c) where readPrec = failRead
deriving instance ( GHC.Generic (f c)
                  , Serialise   (f c)) => Serialise   (Field Still f c)
deriving instance   Show        (f c)  => Show        (Field Still f c)
