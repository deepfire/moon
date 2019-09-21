{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Moon.Face
  ( -- *
    Con(..)
  , Repr
  , Tag(..)
  , Tag'(..)
  , pxType
  , tagType
  , Type(..)
  , TypeName(..)
  , PipeName(..)
  , Variant(..)
  , VariantName(..)
  , Field(..)
  , FieldName(..)
  , Sig(..)
  , Struct(..)
  , Def(..)
  , Scope(..)
  , Value(..)
  , mkValue
  , mkValue'
  , SomeKindValue(..)
  , GroundContext
  , SomeValue(..)
  , someValueSomeTypeRep
  , failRead
    -- * re-exports
  , Dict(..)
  , module Data.RTTI
  , module Data.Some
  )
where

import Debug.Trace
import Text.Printf

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Data.Map                           (Map)
import           Data.String                        (IsString)
import           Data.Text                          (Text, pack)
import           Data.Typeable                      (Typeable)
import qualified Data.Set.Monad                   as S
import qualified Data.Kind                        as Kind
import           Data.Proxy                         (Proxy(..))
import           GHC.Generics                       (Generic)
import           Text.Read
import qualified Type.Reflection                  as R

-- Locals
import Data.Dict
import Data.Orphanage ()
import Data.RTTI
import Data.Some

{-------------------------------------------------------------------------------
  Metatheory
-------------------------------------------------------------------------------}
data Con
  = Point
  | List
  | Set
  | Tree
  | Dag
  | Graph
  deriving (Eq, Generic, Ord, Read, Show, Typeable)
instance Serialise Con

data Tag' (k :: Con) where
  TPoint' :: Tag' Point
  TList'  :: Tag' List
  TSet'   :: Tag' Set
  TTree'  :: Tag' Tree
  TDag'   :: Tag' Dag
  TGraph' :: Tag' Graph
  deriving Typeable

instance Read (Some Tag') where
  readPrec = do
    con <- lexP
    case con of
      Ident "Point" -> pure . Exists $ TPoint'
      Ident "List"  -> pure . Exists $ TList'
      Ident "Set"   -> pure . Exists $ TSet'
      Ident "Tree"  -> pure . Exists $ TTree' 
      Ident "Dag"   -> pure . Exists $ TDag'  
      Ident "Graph" -> pure . Exists $ TGraph'
      _ -> trace (printf "Unknown Tag': %s" (show con))
                 (fail "")

type family Repr k a :: Kind.Type where
  Repr Point a =         a
  Repr List  a =      [] a
  Repr Set   a =   S.Set a
  Repr Tree  a = G.Graph a
  Repr Dag   a = G.Graph a
  Repr Graph a = G.Graph a
  -- Question:  can we somehow avoid introducing higher-kinded types,
  --            so the model can remain simple, without loss of expressivity?
  -- Example:   how do we avoid introducing Map a b?
  -- Survey:    what seems to require Map a b?
  -- Question2: what are other options for representing that using
  --            that which already exists?
  -- Option:    Set of pairs with enforced left hand uniqueness?

-- | 'Tag' is a richer, internal representation form of 'Type'.
data Tag   (k :: Con) a where
  TPoint  :: Tag Point a
  TList   :: Tag List  a
  TSet    :: Tag Set   a
  TTree   :: Tag Tree  a
  TDag    :: Tag Dag   a
  TGraph  :: Tag Graph a
  deriving (Typeable)


pxType  :: forall k a. (Typeable k, Typeable a) => Proxy k -> Proxy a -> Type
pxType _ pa =
  Type (TypeName . pack . R.tyConName $ R.someTypeRepTyCon tr)
       (R.typeRepTyCon $ R.typeRep @k)
       tr
  where tr = R.someTypeRep pa

tagType :: forall k a. Typeable a => Tag k a -> Type
tagType TPoint = pxType (Proxy @k) (Proxy @a)
tagType TList  = pxType (Proxy @k) (Proxy @a)
tagType TSet   = pxType (Proxy @k) (Proxy @a)
tagType TTree  = pxType (Proxy @k) (Proxy @a)
tagType TDag   = pxType (Proxy @k) (Proxy @a)
tagType TGraph = pxType (Proxy @k) (Proxy @a)

--------------------------------------------------------------------------------
-- | TypeName: unique identifier of a type constructor
newtype TypeName = TypeName Text  deriving (Eq, Generic, Ord, IsString, Read, Serialise, Show)

-- | 'Type' is a serialisable form of 'Tag'.
data Type =
  Type
  { tName :: TypeName       -- ^ Extracted from the typerep
  , tCon  :: R.TyCon        -- ^ Con
  , tType :: R.SomeTypeRep  -- ^ Kind.Type
  } deriving (Eq, Generic, Ord)
instance Serialise Type

failRead :: forall a. Typeable a => ReadPrec a
failRead = error $ "Cannot read instances of "<>show (R.typeRep @a)<>".  Sorry."

instance Read Type where readPrec = failRead

--------------------------------------------------------------------------------
-- | PipeName: unique identifier of a Pipe withina Scope.
newtype PipeName = PipeName Text  deriving (Eq, Generic, Ord, IsString, Read, Serialise, Show)

--------------------------------------------------------------------------------
-- | VariantName: unique identifier of a type constructor
newtype VariantName = VariantName Text  deriving (Eq, Generic, Ord, IsString, Read, Serialise, Show)

-- | 'Variant' of an ADT
data Variant
  = Variant
    { vName   :: VariantName
    , vFields :: [Field]
    } deriving (Eq, Generic, Ord, Show)
instance Serialise Variant
instance Read Variant where readPrec = failRead

--------------------------------------------------------------------------------
-- | FieldName: name of a 'Variant's field
newtype FieldName = FieldName Text  deriving (Eq, Generic, Ord, IsString, Serialise, Read, Show)

-- | 'Field' of a 'Proj'
data Field
  = Field
    { fName :: FieldName
    , fType :: Type
    } deriving (Eq, Generic, Ord, Show)
instance Serialise Field

instance Read Field where readPrec = failRead

--------------------------------------------------------------------------------
-- | Sig: a structure-oblivious abstraction of a pipe as its endpoints.
data Sig
  = Gen    -- ^  Pipe endpoint: IO a
    { sOut :: Type
    }
  | Link   -- ^ Pipe transform: b → IO c
    { sIn  :: Type
    , sOut :: Type
    }
  deriving (Eq, Generic, Ord)
instance Serialise Sig
instance Read Sig where readPrec = failRead

--------------------------------------------------------------------------------
-- | Struct: Pipe's definition, as a graph of type transformations.
newtype Struct = Struct (G.Graph Type) deriving (Eq, Generic, Ord, Show)
instance Serialise Struct
instance Read Struct where readPrec = failRead

--------------------------------------------------------------------------------
-- | Def: element of Scope.
data Def = Def { dPipeName :: PipeName, dSig :: Sig, dStruct  :: Struct } deriving (Eq, Generic, Ord, Show)
instance Serialise Def
instance Read Def where readPrec = failRead

--------------------------------------------------------------------------------
-- | Scope: define range of 'Struct' as uniquely-'Name'd
--   set of signatures and structures.
--   Does not contain executable code, so can be exchanged over the wire.
newtype Scope = Scope { unScope :: Map PipeName Def } deriving (Eq, Generic, Ord, Show)
instance Serialise Scope
instance Read Scope where readPrec = failRead

--------------------------------------------------------------------------------
-- | Value:  result of running a 'Pipe'.
data Value (k :: Con) a where
  VPoint :: Repr Point a -> Value Point a
  VList  :: Repr List  a -> Value List  a
  VSet   :: Repr Set   a -> Value Set   a
  VTree  :: Repr Tree  a -> Value Tree  a
  VDag   :: Repr Dag   a -> Value Dag   a
  VGraph :: Repr Graph a -> Value Graph a
  deriving (Typeable)

instance Functor (Value k) where
  fmap f (VPoint x) = VPoint $ f x
  fmap f (VList x) = VList $ f <$> x
  fmap f (VSet x) = VSet $ f <$> x
  fmap f (VTree x) = VTree $ f <$> x
  fmap f (VDag x) = VDag $ f <$> x
  fmap f (VGraph x) = VGraph $ f <$> x

mkValue' :: Tag' k -> Repr k a -> Value k a
mkValue' = \case
  TPoint' -> VPoint
  TList'  -> VList
  TSet'   -> VSet
  TTree'  -> VTree
  TDag'   -> VDag
  TGraph' -> VGraph

mkValue :: Tag k a -> Repr k a -> Value k a
mkValue = \case
  TPoint -> VPoint
  TList  -> VList
  TSet   -> VSet
  TTree  -> VTree
  TDag   -> VDag
  TGraph -> VGraph

--------------------------------------------------------------------------------
-- * Various degrees of type detail loss.
--
data SomeKindValue a = forall (k :: Con). Typeable k => SomeKindValue (Value k a)

type     GCtx a = (Ord a, Typeable a, Serialise a, Read a, Show a)
class    GCtx a => GroundContext a
instance GCtx a => GroundContext a

data SomeValue = forall a. GroundContext a => SomeValue  (SomeKindValue a)

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue (_ :: SomeKindValue a)) = R.someTypeRep $ Proxy @a

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show (Tag' k) where
  show TPoint' = "TPoint"
  show TList'  = "TList"
  show TSet'   = "TSet"
  show TTree'  = "TTree"
  show TDag'   = "TDag"
  show TGraph' = "TGraph"
instance Show (Tag k a) where
  show TPoint  = "TPoint"
  show TList   = "TList"
  show TSet    = "TSet"
  show TTree   = "TTree"
  show TDag    = "TDag"
  show TGraph  = "TGraph"
instance Show Type where
  show (Type _ tycon sometyperep) = "Type "<>show tycon<>" "<>show sometyperep
instance Show Sig where
  show (Gen    o)  =  "Gen "<>show o
  show (Link i o)  = "Link "<>show i<>" -> "<>show o

instance (Ord a, Show a) => Show (Value k a) where
  show (VPoint x) = "VPoint " <> show x
  show (VList  x) = "VList "  <> show x
  show (VSet   x) = "VSet "   <> show x
  show (VTree  x) = "VTree "  <> show x
  show (VDag   x) = "VDag "   <> show x
  show (VGraph x) = "VGraph " <> show x

instance (Ord a, Show a) => Show (SomeKindValue a) where
  show (SomeKindValue x) = "(SKV "<>show x<>")"

instance Show SomeValue where
  show (SomeValue (SomeKindValue x)) = show x
