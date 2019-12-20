{-# LANGUAGE UndecidableSuperClasses #-}
module Type
  ( Name(..)
  , QName(..)
  , qname
  , append
  , prepend
  , textQName
  , listQName
  , showQName
  , coerceName
  , coerceQName
  , Con(..)
  , Tag(..)
  , withReifyTag
  , SomeTag(..)
  , ReifyTag(..)
  , Type(..)
  , Repr
  , mapRepr
  , TypePair(..)
  , OnArg1
  , Arg1
  , Arg1Tag
  , Arg1Ty
  , WithPair
  , TagOf
  , TypeOf
  , ReprOf
  , someType
  , SomeType(..)
  , proxySomeType
  , tagSomeType
  , unitSomeType
  , showSomeType
  , Value(..)
  , mkValue
  , mkValue'
  , Parse(..)
  , Ground
  , GroundData
  , GroundDataFull
  , SomeKindValue(..)
  , SomeValue(..)
  , someValueSomeTypeRep
  , withSomeValue
  , Representable(..)
  -- * Re-exports
  , Some(..)
  , module Basis
  , module Data.Some
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import qualified Codec.CBOR.Decoding              as CBOR (decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (encodeWord)
import           Control.DeepSeq                    (NFData(..))
import qualified Data.Sequence                    as Seq
import qualified Data.Set.Monad                   as S
import qualified Data.Text                        as Text
import           Data.Text                          (split)
import           Data.String                        (IsString(..))
import           GHC.Generics                       (Generic)
import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty
import           Text.Parser.Token                  (TokenParsing)
import           Text.Read                          (Read(..), Lexeme(..), lexP)
import qualified Type.Reflection                  as R
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
  deriving (Eq, Generic, Ord, Read, Serialise, Typeable)

newtype QName a = QName (Seq (Name a))
  deriving (Eq, Generic, Ord,           Read, Serialise, Typeable)

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
data Tag (k :: Con) where
  TPoint :: Tag Point
  TList  :: Tag List
  TSet   :: Tag 'Set
  TTree  :: Tag Tree
  TDag   :: Tag Dag
  TGraph :: Tag Graph
  deriving (Typeable)

class ReifyTag (k :: Con) where
  reifyTag :: Proxy k -> Tag k

instance ReifyTag Point where reifyTag = const TPoint
instance ReifyTag List  where reifyTag = const TList
instance ReifyTag 'Set  where reifyTag = const TSet
instance ReifyTag Tree  where reifyTag = const TTree
instance ReifyTag Dag   where reifyTag = const TDag
instance ReifyTag Graph where reifyTag = const TGraph

instance NFData (Tag k) where
  rnf TPoint = ()
  rnf _      = ()

instance Eq (Tag k) where
  TPoint == TPoint = True
  TList  == TList  = True
  TSet   == TSet   = True
  TTree  == TTree  = True
  TDag   == TDag   = True
  TGraph == TGraph = True

instance Ord (Tag k) where
  compare TPoint TPoint = EQ
  compare TList  TList  = EQ
  compare TSet   TSet   = EQ
  compare TTree  TTree  = EQ
  compare TDag   TDag   = EQ
  compare TGraph TGraph = EQ

--------------------------------------------------------------------------------
data SomeTag where
  SomeTag
    :: (ReifyTag k, Typeable k)
    => Tag (k :: Con) -> SomeTag

instance Serialise SomeTag where
  encode = CBOR.encodeWord . \(SomeTag tag) -> case tag of
    TPoint -> 1
    TList  -> 2
    TSet   -> 3
    TTree  -> 4
    TDag   -> 5
    TGraph -> 6
  decode = do
    tag <- CBOR.decodeWord
    case tag of
      1 -> pure $ SomeTag TPoint
      2 -> pure $ SomeTag TList
      3 -> pure $ SomeTag TSet
      4 -> pure $ SomeTag TTree
      5 -> pure $ SomeTag TDag
      6 -> pure $ SomeTag TGraph
      _ -> fail $ "invalid SomeTag encoding: tag="<>show tag

--------------------------------------------------------------------------------
data Type (k :: Con) (a :: *) where
  TPoint'  :: Type Point a
  TList'   :: Type List  a
  TSet'    :: Type 'Set  a
  TTree'   :: Type Tree  a
  TDag'    :: Type Dag   a
  TGraph'  :: Type Graph a
  deriving (Typeable)

splitType :: forall k a. ReifyTag k => Type k a -> (Tag k, Proxy a)
splitType _ = (,) (reifyTag $ Proxy @k) (Proxy @a)

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

mapRepr :: Tag k -> (a -> b) -> Repr k a -> Repr k b
mapRepr TPoint f = f
mapRepr TList  f = fmap f
mapRepr TSet   f = fmap f
mapRepr TTree  f = fmap f
mapRepr TDag   f = fmap f
mapRepr TGraph f = fmap f

withReifyTag :: Tag k -> (ReifyTag k => r) -> r
withReifyTag = \case
  TPoint -> id
  TList  -> id
  TSet   -> id
  TTree  -> id
  TDag   -> id
  TGraph -> id

--------------------------------------------------------------------------------
data family TypePair t :: *

data instance TypePair ty where
  TypePair :: (ty ~ Type k a, ReifyTag k, Typeable k, Typeable a) =>
    { tpTag  :: Tag k
    , tpType :: Proxy a
    } -> TypePair (Type k a)

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

type family Arg1Tag (arg1tag :: [*]) :: Con where
  Arg1Tag (Type k _:_) = k
  Arg1Tag xs = TypeError (Ty.Text "Arg1Tag: no argument: " :<>: ShowType xs)

type family TypeOf (typeof :: *) :: * where
  TypeOf (Type _ a) = a
  TypeOf x = TypeError (Ty.Text "TypeOf: invalid argument: " :<>: ShowType x)

type family TagOf (tagof :: *) :: Con where
  TagOf (Type k _) = k
  TagOf  x = TypeError (Ty.Text "TagOf: invalid argument: " :<>: ShowType x)

type WithPair (ty :: *) (k :: Con) (a :: *)
  = (TagOf ty ~ k, TypeOf ty ~ a)

type family ReprOf (reprof :: *) :: * where
  ReprOf (TypePair (Type k a)) = Repr k a
  ReprOf x = TypeError (Ty.Text "ReprOf: invalid argument: " :<>: ShowType x)

--------------------------------------------------------------------------------
-- | 'SomeType' is a serialisable form of 'Type'
data SomeType =
  SomeType
  { tName :: Name SomeType  -- ^ Extracted from the typerep
  , tCon  :: R.TyCon        -- ^ Con
  , tRep  :: R.SomeTypeRep  -- ^ Kind.Type
  } deriving (Eq, Generic, Ord)

someType :: forall k a. (Typeable k, Typeable a) => Type k a -> SomeType
someType _ =
  SomeType (Name . pack . R.tyConName $ R.someTypeRepTyCon tr)
       (R.typeRepTyCon $ R.typeRep @k)
       tr
  where tr = R.someTypeRep $ Proxy @a

unitSomeType :: SomeType
unitSomeType = tagSomeType TPoint (Proxy @())

proxySomeType  :: forall k a. (Typeable k, Typeable a) => Proxy k -> Proxy a -> SomeType
proxySomeType _ pa =
  SomeType (Name . pack . R.tyConName $ R.someTypeRepTyCon tr)
       (R.typeRepTyCon $ R.typeRep @k)
       tr
  where tr = R.someTypeRep pa

tagSomeType :: forall k a. Typeable a => Tag k -> Proxy a -> SomeType
tagSomeType TPoint a = proxySomeType (Proxy @k) a
tagSomeType TList  a = proxySomeType (Proxy @k) a
tagSomeType TSet   a = proxySomeType (Proxy @k) a
tagSomeType TTree  a = proxySomeType (Proxy @k) a
tagSomeType TDag   a = proxySomeType (Proxy @k) a
tagSomeType TGraph a = proxySomeType (Proxy @k) a

showSomeType :: SomeType -> Text
showSomeType SomeType{tName=(showName -> n), tCon} =
  case R.tyConName tCon of
    "'Point" -> "• "<>n
    "'List"  -> "["<>n<>"]"
    "'Set"   -> "{"<>n<>"}"
    "'Tree"  -> "♆⇊ "<>n
    "'Dag"   -> "♆⇄ "<>n
    "'Graph" -> "☸ "<>n

--------------------------------------------------------------------------------
data Value (k :: Con) a where
  VPoint  :: Repr Point a -> Value Point a
  VList   :: Repr List  a -> Value List  a
  VSet    :: Repr 'Set  a -> Value 'Set  a
  VTree   :: Repr Tree  a -> Value Tree  a
  VDag    :: Repr Dag   a -> Value Dag   a
  VGraph  :: Repr Graph a -> Value Graph a
  deriving (Typeable)

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

--------------------------------------------------------------------------------
-- * Ground
--
data SomeKindValue a =
  forall (k :: Con). Typeable k
  => SomeKindValue (Tag k) (Value k a)

type     GCtx a = (Ord a, Typeable a, Serialise a, Parse a, Read a, Show a)
class    GCtx a => Ground a
instance GCtx a => Ground a

class    (Ground a, HasTypeData Ground a) => GroundData a
instance (Ground a, HasTypeData Ground a) => GroundData a

class    ( Ground a, HasTypeData Ground a
         , All2 Ground (Code a)
         ) => GroundDataFull a
instance ( Ground a, HasTypeData Ground a
         , All2 Ground (Code a)
         ) => GroundDataFull a

data SomeValue = forall a. Ground a => SomeValue  (SomeKindValue a)

withSomeValue
  :: forall a k b
   . (Ground a, Typeable k)
  => Tag k
  -> Proxy a
  -> SomeValue
  -> (Value k a -> b)
  -> Either Text b
withSomeValue _ _ (SomeValue (SomeKindValue (_ :: Tag k') r :: SomeKindValue a')) f =
  let exptr = typeRep @a
      svtr  = typeRep @a'
      expk  = typeRep @k
      svk   = typeRep @k'
  in case (,) (svtr `R.eqTypeRep` exptr)
              (svk  `R.eqTypeRep` expk) of
    (Just R.HRefl, Just R.HRefl) -> Right $ f r
    _ -> Left . pack $ printf "withSomeValue: expected %s/%s, got %s/%s"
                (show exptr) (show expk) (show svtr) (show svk)

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue (_ :: SomeKindValue a)) = R.someTypeRep $ Proxy @a

--------------------------------------------------------------------------------
-- * Generic parser
--
class Parse a where
  -- parser :: (MonadParsec Text Text m, TokenParsing m) => m a
  parser :: Parser a

instance {-# OVERLAPPABLE #-} Typeable a => Parse a where
  parser = failParser
    where
      failParser :: (Monad m, TokenParsing m) => m a
      failParser = fail ("No parser for " <> show (someTypeRep $ Proxy @a) <> ".")

--------------------------------------------------------------------------------
-- * Representable
--
class Representable (a :: *) where
  type Present a :: *
  repr :: a -> Present a

instance {-# OVERLAPPABLE #-} Representable a where
  type instance Present a = a
  repr = id

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

instance Show (Type k a) where
  show TPoint'  = "TPoint"
  show TList'   = "TList"
  show TSet'    = "TSet"
  show TTree'   = "TTree"
  show TDag'    = "TDag"
  show TGraph'  = "TGraph"

instance Read SomeType where readPrec = failRead
instance Show SomeType where
  show (SomeType _ tycon sometyperep) =
    show tycon<>":"<>unpack
    -- cut out the middle part of a name: we don't care about the kind
    (if Text.isPrefixOf "Name"  shownRep ||
        Text.isPrefixOf "QName" shownRep
     then Text.takeWhile (/= ' ') shownRep <> " " <>
          (Text.reverse . Text.takeWhile (/= ' ') . Text.reverse $ shownRep)
     else shownRep)
    where shownRep = pack $ show sometyperep
instance Serialise SomeType

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

instance (Ord a, Show a) => Show (SomeKindValue a) where
  show (SomeKindValue _ x) = "(SKV "<>show x<>")"

instance Show SomeValue where
  show (SomeValue (SomeKindValue _ x)) = show x
