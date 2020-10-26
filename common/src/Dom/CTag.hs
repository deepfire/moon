{-# LANGUAGE UndecidableInstances       #-}
module Dom.CTag (module Dom.CTag) where

import qualified Algebra.Graph                    as G
import qualified Codec.CBOR.Decoding              as CBOR (decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (encodeWord)
import           Codec.Serialise                    (Serialise(..))
import           Control.DeepSeq                    (NFData(..))
import           Control.Monad.Fail                 (MonadFail)
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import qualified Data.Set.Monad                   as S
import qualified Data.Text                        as Text
import           Data.Typeable                      (Proxy, Typeable, (:~:)(..), (:~~:)(..))
import qualified Generics.SOP                     as SOP
import           GHC.Generics                       (Generic)
import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty

import Data.Parsing
import Dom.Some


--------------------------------------------------------------------------------
-- * Types and classes
--
data Con
  = Point
  | List
  | Set
  | Tree
  | Dag
  | Graph
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

data CTag (c :: Con) where
  TPoint :: CTag Point
  TList  :: CTag List
  TSet   :: CTag 'Set
  TTree  :: CTag Tree
  TDag   :: CTag Dag
  TGraph :: CTag Graph
  deriving (Typeable)

data Types (c :: Con) (a :: *) where
  TPoint'  :: Types Point a
  TList'   :: Types List  a
  TSet'    :: Types 'Set  a
  TTree'   :: Types Tree  a
  TDag'    :: Types Dag   a
  TGraph'  :: Types Graph a
  deriving (Typeable)

data SomeCTag where
  SomeCTag
    :: (ReifyCTag c, Typeable c)
    => CTag (c :: Con) -> SomeCTag

class ReifyCTag (c :: Con) where
  reifyCTag :: Proxy c -> CTag c

--------------------------------------------------------------------------------
-- * Type-level structures
--
type family OnArg1 (onarg1f :: Con -> * -> *) (onarg1xs :: [*]) :: * where
  OnArg1 f (Types c a:_) = f c a
  OnArg1 _ xs = TypeError (Ty.Text "OnArg1Ty: incompatible signature: " :<>: ShowType xs)

type family Arg1 (arg1 :: [*]) :: * where
  Arg1 (Types c a:_) = Types c a
  Arg1 xs = TypeError (Ty.Text "Arg1: no argument: " :<>: ShowType xs)

type family Arg1Ty (arg1ty :: [*]) :: * where
  Arg1Ty (Types _ a:_) = a
  Arg1Ty xs = TypeError (Ty.Text "Arg1Ty: no argument: " :<>: ShowType xs)

type family Arg1CTag (arg1ctag :: [*]) :: Con where
  Arg1CTag (Types c _:_) = c
  Arg1CTag xs = TypeError (Ty.Text "Arg1CTag: no argument: " :<>: ShowType xs)

type family TypeOf (typeof :: *) :: * where
  TypeOf (Types _ a) = a
  TypeOf x = TypeError (Ty.Text "TypeOf: invalid argument: " :<>: ShowType x)

type family CTagOf (ctagof :: *) :: Con where
  CTagOf (Types c _) = c
  CTagOf  x = TypeError (Ty.Text "CTagOf: invalid argument: " :<>: ShowType x)

type WithPair (ty :: *) (c :: Con) (a :: *)
  = (CTagOf ty ~ c, TypeOf ty ~ a)

type family ReprOf (reprof :: *) :: * where
  ReprOf (Types c a) = Repr c a
  ReprOf x = TypeError (Ty.Text "ReprOf: invalid argument: " :<>: ShowType x)

--------------------------------------------------------------------------------
-- * Instances
--
instance SOP.Generic         Con
instance SOP.HasDatatypeInfo Con
instance Serialise           Con

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

instance Show (CTag c) where
  show TPoint = "TPoint"
  show TList  = "TList"
  show TSet   = "TSet"
  show TTree  = "TTree"
  show TDag   = "TDag"
  show TGraph = "TGraph"

instance NFData (CTag c) where
  rnf TPoint = ()
  rnf _      = ()

instance GEq CTag where
  geq a b = case (a,b) of
    (,) TPoint  TPoint -> Just Refl
    (,) TList   TList  -> Just Refl
    (,) TSet    TSet   -> Just Refl
    (,) TTree   TTree  -> Just Refl
    (,) TDag    TDag   -> Just Refl
    (,) TGraph  TGraph -> Just Refl
    _ -> Nothing

instance GCompare CTag where
  gcompare a b = case geq a b of
    Just Refl -> GEQ
    Nothing -> case orderCTag a `compare` orderCTag b of
      LT -> GLT
      GT -> GGT
   where
     orderCTag :: forall a. CTag a -> Int
     orderCTag = \case
       TPoint -> 0
       TList  -> 1
       TSet   -> 2
       TTree  -> 3
       TDag   -> 4
       TGraph -> 5

instance ReifyCTag Point where reifyCTag = const TPoint
instance ReifyCTag List  where reifyCTag = const TList
instance ReifyCTag 'Set  where reifyCTag = const TSet
instance ReifyCTag Tree  where reifyCTag = const TTree
instance ReifyCTag Dag   where reifyCTag = const TDag
instance ReifyCTag Graph where reifyCTag = const TGraph

instance Serialise SomeCTag where
  encode = CBOR.encodeWord . \(SomeCTag tag) -> case tag of
    TPoint -> 1
    TList  -> 2
    TSet   -> 3
    TTree  -> 4
    TDag   -> 5
    TGraph -> 6
  decode = do
    tag <- CBOR.decodeWord
    case tag of
      1 -> pure $ SomeCTag TPoint
      2 -> pure $ SomeCTag TList
      3 -> pure $ SomeCTag TSet
      4 -> pure $ SomeCTag TTree
      5 -> pure $ SomeCTag TDag
      6 -> pure $ SomeCTag TGraph
      _ -> fail $ "invalid SomeCTag encoding: tag="<>show tag

--------------------------------------------------------------------------------
-- * Mapping to representation
--
type family Repr (k :: Con) (a :: *) :: * where
  Repr Point a =            a
  Repr List  a =         [] a
  Repr 'Set  a =         [] a
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

mapRepr :: CTag c -> (a -> b) -> Repr c a -> Repr c b
mapRepr TPoint f = f
mapRepr TList  f = fmap f
mapRepr TSet   f = fmap f
mapRepr TTree  f = fmap f
mapRepr TDag   f = fmap f
mapRepr TGraph f = fmap f

--------------------------------------------------------------------------------
-- * Utilities
--
withReifyCTag :: CTag c -> (ReifyCTag c => r) -> r
withReifyCTag = \case
  TPoint -> id
  TList  -> id
  TSet   -> id
  TTree  -> id
  TDag   -> id
  TGraph -> id

parseCTag
  :: forall m
  . (MonadFail m, TokenParsing m)
  => m (Some CTag)
parseCTag = do
  i <- ctagIdentifier
  case i of
    "Point" -> pure $ Exists TPoint
    "List"  -> pure $ Exists TList
    "Set"   -> pure $ Exists TSet
    "Tree"  -> pure $ Exists TTree
    "Dag"   -> pure $ Exists TDag
    "Graph" -> pure $ Exists TGraph
    x -> fail $ "Mal-Con: " <> show x
 where
   ctagIdentifier :: (Monad m, TokenParsing m) => m Text.Text
   ctagIdentifier = ident $ IdentifierStyle
     { _styleName = "Con tag"
     , _styleStart = letter
     , _styleLetter = alphaNum
     , _styleReserved = mempty
     , _styleHighlight = Identifier
     , _styleReservedHighlight = ReservedIdentifier
     }
