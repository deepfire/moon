{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Type
  ( Name(..)
  , QName(..)
  , Located(..)
  , qname
  , append
  , prepend
  , lastQName
  , listQName
  , showQName
  , textQName
  , unconsQName
  , coerceName
  , coerceQName
  , Con(..)
  , CTag(..)
  , withReifyCTag
  , ReifyCTag(..)
  , Type(..)
  , splitType
  , Repr
  , mapRepr
  , TypePair(..)
  , OnArg1
  , Arg1
  , Arg1CTag
  , Arg1Ty
  , WithPair
  , CTagOf
  , TypeOf
  , ReprOf
  , Value(..)
  , mkValue
  , mkValue'
  , Parse(..)
  , Ground
  , GroundData
  , GroundDataFull
  , Representable(..)
  -- * Re-exports
  , Interval(..)
  , Some(..)
  , module Basis
  , module Data.Some
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Control.Monad.Fail                 (MonadFail)
import           Data.IntervalMap.FingerTree (Interval(..))
import qualified Data.Sequence                    as Seq
import qualified Data.Set.Monad                   as S
import qualified Data.Text                        as Text
import           Data.Text                          (split)
import           Data.String                        (IsString(..))
import           GHC.Generics                       (Generic)
import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty
import           Text.Parser.Token                  (TokenParsing)
import           Text.Read                          (Lexeme(..), lexP)
import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Data.Parsing
import Data.Some
import Generics.SOP.Some hiding (Generic)
import qualified Generics.SOP.Some as SOP

{-------------------------------------------------------------------------------
  Metatheory
-------------------------------------------------------------------------------}
newtype  Name a  = Name { showName :: Text }
  deriving (Eq, Generic, NFData, Ord, Read, Serialise, Typeable)

newtype QName a = QName { unQName :: Seq (Name a) }
  deriving (Eq, Generic, Ord,           Read, Serialise, Typeable)

-- TODO:  consider using a single Loc/Located type
data Located a
  = Locn
    { locSpan :: {-# UNPACK #-} !(Interval Int)
    , locVal  :: !a
    }
  deriving (Functor)

instance Show a => Show (Located a) where
  show = show . locVal

instance Show (Name a)  where show = unpack . showName
instance Show (QName a) where show = unpack . showQName

instance IsString (Name  a) where fromString = Name . pack
instance IsString (QName a) where fromString = textQName . pack

instance Semigroup (QName a) where
  QName l <> QName r = QName (l <> r)

instance Monoid (QName a) where
  mempty = QName mempty

qname :: Name a -> QName a
qname = QName . Seq.singleton

append :: QName a -> Name a -> QName a
append (QName xs) x = QName $ xs Seq.|> x

prepend :: Name a -> QName a -> QName a
prepend x (QName xs) = QName $ x Seq.<| xs

unconsQName :: QName a -> Maybe (QName a, Name a)
unconsQName (QName xs) = case Seq.viewr xs of
  Seq.EmptyR -> Nothing
  pfx Seq.:> n -> Just (QName pfx, n)

lastQName :: QName a -> Name a
lastQName q@(QName xs) = case Seq.viewr xs of
  Seq.EmptyR -> error $ "lastQName:  invoked on" <> show q
  _ Seq.:> n -> n

textQName :: Text -> QName a
textQName = QName . (Name <$>) . Seq.fromList . split (== '.')

listQName :: [Name a] -> QName a
listQName = QName . Seq.fromList

showQName :: QName a -> Text
showQName (QName s) = Text.intercalate "." $ flip fmap (toList s) $
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
data CTag (c :: Con) where
  TPoint :: CTag Point
  TList  :: CTag List
  TSet   :: CTag 'Set
  TTree  :: CTag Tree
  TDag   :: CTag Dag
  TGraph :: CTag Graph
  deriving (Typeable)

class ReifyCTag (c :: Con) where
  reifyCTag :: Proxy c -> CTag c

instance ReifyCTag Point where reifyCTag = const TPoint
instance ReifyCTag List  where reifyCTag = const TList
instance ReifyCTag 'Set  where reifyCTag = const TSet
instance ReifyCTag Tree  where reifyCTag = const TTree
instance ReifyCTag Dag   where reifyCTag = const TDag
instance ReifyCTag Graph where reifyCTag = const TGraph

instance NFData (CTag c) where
  rnf TPoint = ()
  rnf _      = ()

instance Eq (CTag c) where
  TPoint == TPoint = True
  TList  == TList  = True
  TSet   == TSet   = True
  TTree  == TTree  = True
  TDag   == TDag   = True
  TGraph == TGraph = True

instance Ord (CTag c) where
  compare TPoint TPoint = EQ
  compare TList  TList  = EQ
  compare TSet   TSet   = EQ
  compare TTree  TTree  = EQ
  compare TDag   TDag   = EQ
  compare TGraph TGraph = EQ

--------------------------------------------------------------------------------
data Type (c :: Con) (a :: *) where
  TPoint'  :: Type Point a
  TList'   :: Type List  a
  TSet'    :: Type 'Set  a
  TTree'   :: Type Tree  a
  TDag'    :: Type Dag   a
  TGraph'  :: Type Graph a
  deriving (Typeable)

splitType :: forall c a. ReifyCTag c => Type c a -> (CTag c, Proxy a)
splitType _ = (,) (reifyCTag $ Proxy @c) (Proxy @a)

--------------------------------------------------------------------------------
type family Repr (k :: Con) (a :: *) :: * where
  Repr Point a =            a
  Repr List  a =         [] a
  Repr 'Set  a =      S.Set a
  Repr Tree  a =    G.Graph a
  Repr Dag   a =    G.Graph a
  Repr Graph a =    G.Graph a
  -- Question:  can we somehow avoid introducing higher-kinded types,
  --            so the model can remain simple, without loss of expressivity?
  -- Example:   how do we avoid introducing Map a b?
  -- Survey:    what seems to require Map a b?
  -- Question2: what are other options for representing that using
  --            that which already exists?
  -- Option:    Set of pairs with enforced left hand uniqueness?

mapRepr :: CTag k -> (a -> b) -> Repr k a -> Repr k b
mapRepr TPoint f = f
mapRepr TList  f = fmap f
mapRepr TSet   f = fmap f
mapRepr TTree  f = fmap f
mapRepr TDag   f = fmap f
mapRepr TGraph f = fmap f

withReifyCTag :: CTag k -> (ReifyCTag k => r) -> r
withReifyCTag = \case
  TPoint -> id
  TList  -> id
  TSet   -> id
  TTree  -> id
  TDag   -> id
  TGraph -> id

--------------------------------------------------------------------------------
data family TypePair t :: *

data instance TypePair ty where
  TypePair :: (ty ~ Type k a, ReifyCTag k, Typeable k, Typeable a) =>
    { tpCTag :: CTag k
    , tpType :: Proxy a
    } -> TypePair (Type k a)

deriving instance Eq       (TypePair t)
deriving instance Ord      (TypePair t)
deriving instance Typeable (TypePair t)

instance NFData (TypePair a) where
  rnf _ = ()

type family OnArg1 (onarg1f :: Con -> * -> *) (onarg1xs :: [*]) :: * where
  OnArg1 f (Type k a:_) = f k a
  OnArg1 _ xs = TypeError (Ty.Text "OnArg1Ty: incompatible signature: " :<>: ShowType xs)

type family Arg1 (arg1 :: [*]) :: * where
  Arg1 (Type k a:_) = Type k a
  Arg1 xs = TypeError (Ty.Text "Arg1: no argument: " :<>: ShowType xs)

type family Arg1Ty (arg1ty :: [*]) :: * where
  Arg1Ty (Type _ a:_) = a
  Arg1Ty xs = TypeError (Ty.Text "Arg1Ty: no argument: " :<>: ShowType xs)

type family Arg1CTag (arg1ctag :: [*]) :: Con where
  Arg1CTag (Type k _:_) = k
  Arg1CTag xs = TypeError (Ty.Text "Arg1CTag: no argument: " :<>: ShowType xs)

type family TypeOf (typeof :: *) :: * where
  TypeOf (Type _ a) = a
  TypeOf x = TypeError (Ty.Text "TypeOf: invalid argument: " :<>: ShowType x)

type family CTagOf (tagof :: *) :: Con where
  CTagOf (Type c _) = c
  CTagOf  x = TypeError (Ty.Text "CTagOf: invalid argument: " :<>: ShowType x)

type WithPair (ty :: *) (k :: Con) (a :: *)
  = (CTagOf ty ~ k, TypeOf ty ~ a)

type family ReprOf (reprof :: *) :: * where
  ReprOf (Type k a) = Repr k a
  ReprOf x = TypeError (Ty.Text "ReprOf: invalid argument: " :<>: ShowType x)

--------------------------------------------------------------------------------
data Value (k :: Con) a where
  VPoint  :: Repr Point a -> Value Point a
  VList   :: Repr List  a -> Value List  a
  VSet    :: Repr 'Set  a -> Value 'Set  a
  VTree   :: Repr Tree  a -> Value Tree  a
  VDag    :: Repr Dag   a -> Value Dag   a
  VGraph  :: Repr Graph a -> Value Graph a
  deriving (Typeable)

mkValue' :: Proxy a -> CTag k -> Repr k a -> Value k a
mkValue' = const $ \case
  TPoint -> VPoint
  TList  -> VList
  TSet   -> VSet
  TTree  -> VTree
  TDag   -> VDag
  TGraph -> VGraph

mkValue :: Proxy a -> CTag k -> Repr k a -> Value k a
mkValue = const $ \case
  TPoint -> VPoint
  TList  -> VList
  TSet   -> VSet
  TTree  -> VTree
  TDag   -> VDag
  TGraph -> VGraph

--------------------------------------------------------------------------------
-- * Ground
--
type     GroundCtx a = (Ord a, Typeable a, Serialise a, Parse a, Read a, Show a)
class    GroundCtx a => Ground a
instance GroundCtx a => Ground a

class    (Ground a, HasTypeData Ground a) => GroundData a
instance (Ground a, HasTypeData Ground a) => GroundData a

class    ( Ground a, HasTypeData Ground a
         , All2 Ground (Code a)
         ) => GroundDataFull a
instance ( Ground a, HasTypeData Ground a
         , All2 Ground (Code a)
         ) => GroundDataFull a

--------------------------------------------------------------------------------
-- * Generic parser
--
class Parse a where
  -- parser :: (MonadParsec Text Text m, TokenParsing m) => m a
  parser :: Parser a

instance {-# OVERLAPPABLE #-} Typeable a => Parse a where
  parser = failParser
    where
      failParser :: (MonadFail m, TokenParsing m) => m a
      failParser = fail ("No parser for " <> show (someTypeRep $ Proxy @a) <> ".")

--------------------------------------------------------------------------------
-- * Representable
--
class Representable (a :: *) where
  type Present a :: *
  repr :: a -> Present a

instance {-# OVERLAPPABLE #-} Representable a where
  type Present a = a
  repr = id

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance SOP.Generic         Con
instance SOP.HasDatatypeInfo Con
instance Serialise           Con

instance Read (Some CTag) where
  readPrec = do
    con <- lexP
    case con of
      Ident "Point" -> pure . Exists $ TPoint
      Ident "List"  -> pure . Exists $ TList
      Ident "Set"   -> pure . Exists $ TSet
      Ident "Tree"  -> pure . Exists $ TTree
      Ident "Dag"   -> pure . Exists $ TDag
      Ident "Graph" -> pure . Exists $ TGraph
      _ -> trace (printf "Unknown CTag: %s" (show con))
                 (fail "")

instance Show (CTag k) where
  show TPoint = "TPoint"
  show TList  = "TList"
  show TSet   = "TSet"
  show TTree  = "TTree"
  show TDag   = "TDag"
  show TGraph = "TGraph"

instance Show (Type k a) where
  show TPoint'  = "TPoint"
  show TList'   = "TList"
  show TSet'    = "TSet"
  show TTree'   = "TTree"
  show TDag'    = "TDag"
  show TGraph'  = "TGraph"

instance (Show a) => Show (Value k a) where
  show (VPoint x) = "VPoint " <> show x
  show (VList  x) = "VList "  <> show x
  show (VSet   x) = "VSet "   <> show (foldMap (:[]) x)
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
