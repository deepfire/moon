{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Type
  ( Name(..)
  , QName(..)
  , qname
  , append
  , prepend
  , (<|), (|>)
  , listQName
  , showName
  , showQName
  , coerceName
  , coerceQName
  , Con(..)
  , Tag(..)
  , ReifyTag(..)
  , Tag2(..)
  , splitTag2
  , Repr
  , Type(..)
  , proxyType
  , tagType
  , Value(..)
  , mkValue
  , mkValue'
  , Parser(..)
  , Ground
  , GroundData
  , SomeKindValue(..)
  , SomeValue(..)
  , someValueSomeTypeRep
  -- * Re-exports
  , Some(..)
  , module Basis
  , module Data.Some
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import qualified Data.Sequence                    as Seq
import qualified Data.Set.Monad                   as S
import qualified Data.Text                        as Text
import           GHC.Generics                       (Generic)
import           Text.Parser.Token                  (TokenParsing)
import           Text.Read                          (Read(..), Lexeme(..), lexP)
import qualified Type.Reflection                  as R
import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Data.Some
import Generics.SOP.Some hiding (Generic)
import qualified Generics.SOP.Some as SOP

{-------------------------------------------------------------------------------
  Metatheory
-------------------------------------------------------------------------------}
newtype  Name a  = Name      Text
  deriving (Eq, Generic, Ord, IsString, Read, Serialise)

newtype QName a = QName (Seq (Name a))
  deriving (Eq, Generic, Ord,           Read, Serialise)

instance Show (Name a)  where show = unpack . showName
instance Show (QName a) where show = unpack . showQName

instance Semigroup (QName a) where
  QName l <> QName r = QName (l <> r)

instance Monoid (QName a) where
  mempty = QName mempty

qname :: Name a -> QName a
qname = QName . Seq.singleton

append, (|>) :: QName a -> Name a -> QName a
append (QName xs) x = QName $ xs Seq.|> x

(|>) = append

prepend, (<|) :: Name a -> QName a -> QName a
prepend x (QName xs) = QName $ x Seq.<| xs

(<|) = prepend

listQName :: [Name a] -> QName a
listQName = QName . Seq.fromList

showName :: Name a -> Text
showName (Name x) = x

showQName :: QName a -> Text
showQName (QName s) = Text.intercalate ":" $ flip fmap (toList s) $
  \(Name x) -> x

coerceName :: Name a -> Name b
coerceName = Unsafe.unsafeCoerce

coerceQName :: QName a -> QName b
coerceQName = Unsafe.unsafeCoerce

--------------------------------------------------------------------------------
data Con
  = Point
  | List
  | Set
  | Tree
  | Dag
  | Graph
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

--------------------------------------------------------------------------------
data Tag (k :: Con) where
  TPoint :: Tag Point
  TList  :: Tag List
  TSet   :: Tag 'Set
  TTree  :: Tag Tree
  TDag   :: Tag Dag
  TGraph :: Tag Graph
  deriving Typeable

data Tag2   (k :: Con) a where
  TPoint'  :: Tag2 Point a
  TList'   :: Tag2 List  a
  TSet'    :: Tag2 'Set  a
  TTree'   :: Tag2 Tree  a
  TDag'    :: Tag2 Dag   a
  TGraph'  :: Tag2 Graph a
  deriving (Typeable)

class ReifyTag (k :: Con) where
  reifyTag :: Proxy k -> Tag k

instance ReifyTag Point where reifyTag = const TPoint
instance ReifyTag List  where reifyTag = const TList
instance ReifyTag 'Set  where reifyTag = const TSet
instance ReifyTag Tree  where reifyTag = const TTree
instance ReifyTag Dag   where reifyTag = const TDag
instance ReifyTag Graph where reifyTag = const TGraph

splitTag2 :: forall k a. ReifyTag k => Tag2 k a -> (Tag k, Proxy a)
splitTag2 _ = (,) (reifyTag $ Proxy @k) (Proxy @a)

--------------------------------------------------------------------------------
type family Repr (k :: Con) (a :: *) :: * where
  Repr Point a =         a
  Repr List  a =      [] a
  Repr 'Set  a =   S.Set a
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

--------------------------------------------------------------------------------
-- | 'Type' is a serialisable form of 'Tag.
data Type =
  Type
  { tName :: Name Type      -- ^ Extracted from the typerep
  , tCon  :: R.TyCon        -- ^ Con
  , tType :: R.SomeTypeRep  -- ^ Kind.Type
  } deriving (Eq, Generic, Ord)

--------------------------------------------------------------------------------
data Value (k :: Con) a where
  VPoint  :: Repr Point a -> Value Point a
  VList   :: Repr List  a -> Value List  a
  VSet    :: Repr 'Set  a -> Value 'Set  a
  VTree   :: Repr Tree  a -> Value Tree  a
  VDag    :: Repr Dag   a -> Value Dag   a
  VGraph  :: Repr Graph a -> Value Graph a
  deriving (Typeable)


-- * Generic parser
--
class Typeable a => Parser a where
  parser :: (Monad m, TokenParsing m) => m a

instance {-# OVERLAPPABLE #-} Typeable a => Parser a where
  parser = failParser
    where
      failParser :: (Monad m, TokenParsing m) => m a
      failParser = fail ("No parser for " <> show (someTypeRep $ Proxy @a) <> ".")

--------------------------------------------------------------------------------
-- * Ground
--
data SomeKindValue a = forall (k :: Con). Typeable k => SomeKindValue (Value k a)

type     GCtx a = (Ord a, Typeable a, Serialise a, Parser a, Read a, Show a)
class    GCtx a => Ground a
instance GCtx a => Ground a

class    (Ground a, HasTypeData Ground a) => GroundData a
instance (Ground a, HasTypeData Ground a) => GroundData a

data SomeValue = forall a. Ground a => SomeValue  (SomeKindValue a)


{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance SOP.Generic         Con
instance SOP.HasDatatypeInfo Con
instance Serialise           Con

instance Read (Some Tag) where
  readPrec = do
    con <- lexP
    case con of
      Ident "Point" -> pure . Exists $ TPoint
      Ident "List"  -> pure . Exists $ TList
      Ident "Set"   -> pure . Exists $ TSet
      Ident "Tree"  -> pure . Exists $ TTree
      Ident "Dag"   -> pure . Exists $ TDag
      Ident "Graph" -> pure . Exists $ TGraph
      _ -> trace (printf "Unknown Tag: %s" (show con))
                 (fail "")
instance Show (Tag k) where
  show TPoint = "TPoint"
  show TList  = "TList"
  show TSet   = "TSet"
  show TTree  = "TTree"
  show TDag   = "TDag"
  show TGraph = "TGraph"

instance Show (Tag2 k a) where
  show TPoint'  = "TPoint"
  show TList'   = "TList"
  show TSet'    = "TSet"
  show TTree'   = "TTree"
  show TDag'    = "TDag"
  show TGraph'  = "TGraph"

proxyType  :: forall k a. (Typeable k, Typeable a) => Proxy k -> Proxy a -> Type
proxyType _ pa =
  Type (Name . pack . R.tyConName $ R.someTypeRepTyCon tr)
       (R.typeRepTyCon $ R.typeRep @k)
       tr
  where tr = R.someTypeRep pa

tagType :: forall k a. Typeable a => Tag k -> Proxy a -> Type
tagType TPoint a = proxyType (Proxy @k) a
tagType TList  a = proxyType (Proxy @k) a
tagType TSet   a = proxyType (Proxy @k) a
tagType TTree  a = proxyType (Proxy @k) a
tagType TDag   a = proxyType (Proxy @k) a
tagType TGraph a = proxyType (Proxy @k) a

instance Read Type where readPrec = failRead
instance Show Type where
  show (Type _ tycon sometyperep) = "Type "<>show tycon<>" "<>show sometyperep
instance Serialise Type

instance (Ord a, Show a) => Show (Value k a) where
  show (VPoint x) = "VPoint " <> show x
  show (VList  x) = "VList "  <> show x
  show (VSet   x) = "VSet "   <> show x
  show (VTree  x) = "VTree "  <> show x
  show (VDag   x) = "VDag "   <> show x
  show (VGraph x) = "VGraph " <> show x

instance Functor (Value k) where
  fmap f (VPoint x) = VPoint $ f x
  fmap f (VList x) = VList $ f <$> x
  fmap f (VSet x) = VSet $ f <$> x
  fmap f (VTree x) = VTree $ f <$> x
  fmap f (VDag x) = VDag $ f <$> x
  fmap f (VGraph x) = VGraph $ f <$> x

mkValue' :: Proxy a -> Tag k -> Repr k a -> Value k a
mkValue' = const $ \case
  TPoint -> VPoint
  TList  -> VList
  TSet   -> VSet
  TTree  -> VTree
  TDag   -> VDag
  TGraph -> VGraph

mkValue :: Proxy a -> Tag k -> Repr k a -> Value k a
mkValue = const $ \case
  TPoint -> VPoint
  TList  -> VList
  TSet   -> VSet
  TTree  -> VTree
  TDag   -> VDag
  TGraph -> VGraph

instance (Ord a, Show a) => Show (SomeKindValue a) where
  show (SomeKindValue x) = "(SKV "<>show x<>")"

instance Show SomeValue where
  show (SomeValue (SomeKindValue x)) = show x

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue (_ :: SomeKindValue a)) = R.someTypeRep $ Proxy @a
