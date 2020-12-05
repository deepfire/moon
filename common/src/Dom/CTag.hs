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
  CPoint :: Typeable Point => CTag Point
  CList  :: Typeable List  => CTag List
  CSet   :: Typeable 'Set  => CTag 'Set
  CTree  :: Typeable Tree  => CTag Tree
  CDag   :: Typeable Dag   => CTag Dag
  CGraph :: Typeable Graph => CTag Graph
  deriving (Typeable)

data CTagV (c :: Con) (a :: *) where
  CVPoint  :: CTagV Point a
  CVList   :: CTagV List  a
  CVSet    :: CTagV 'Set  a
  CVTree   :: CTagV Tree  a
  CVDag    :: CTagV Dag   a
  CVGraph  :: CTagV Graph a
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
  OnArg1 f (CTagV c a:_) = f c a
  OnArg1 _ xs = TypeError (Ty.Text "OnArg1Ty: incompatible signature: " :<>: ShowType xs)

type family Arg1 (arg1 :: [*]) :: * where
  Arg1 (CTagV c a:_) = CTagV c a
  Arg1 xs = TypeError (Ty.Text "Arg1: no argument: " :<>: ShowType xs)

type family Arg1Ty (arg1ty :: [*]) :: * where
  Arg1Ty (CTagV _ a:_) = a
  Arg1Ty xs = TypeError (Ty.Text "Arg1Ty: no argument: " :<>: ShowType xs)

type family Arg1CTag (arg1ctag :: [*]) :: Con where
  Arg1CTag (CTagV c _:_) = c
  Arg1CTag xs = TypeError (Ty.Text "Arg1CTag: no argument: " :<>: ShowType xs)

type family CTagVV (typeof :: *) :: * where
  CTagVV (CTagV _ a) = a
  CTagVV x = TypeError (Ty.Text "CTagVV: invalid argument: " :<>: ShowType x)

type family CTagVC (ctagof :: *) :: Con where
  CTagVC (CTagV c _) = c
  CTagVC  x = TypeError (Ty.Text "CTagVC: invalid argument: " :<>: ShowType x)

type family ReprOf (reprof :: *) :: * where
  ReprOf (CTagV c a) = Repr c a
  ReprOf x = TypeError (Ty.Text "ReprOf: invalid argument: " :<>: ShowType x)

--------------------------------------------------------------------------------
-- * Instances
--
instance SOP.Generic         Con
instance SOP.HasDatatypeInfo Con
instance Serialise           Con

instance Eq (CTag c) where
  CPoint == CPoint = True
  CList  == CList  = True
  CSet   == CSet   = True
  CTree  == CTree  = True
  CDag   == CDag   = True
  CGraph == CGraph = True

instance Ord (CTag c) where
  compare CPoint CPoint = EQ
  compare CList  CList  = EQ
  compare CSet   CSet   = EQ
  compare CTree  CTree  = EQ
  compare CDag   CDag   = EQ
  compare CGraph CGraph = EQ

instance Show (CTag c) where
  show CPoint = "CPoint"
  show CList  = "CList"
  show CSet   = "CSet"
  show CTree  = "CTree"
  show CDag   = "CDag"
  show CGraph = "CGraph"

instance NFData (CTag c) where
  rnf CPoint = ()
  rnf _      = ()

instance GEq CTag where
  geq a b = case (a,b) of
    (,) CPoint  CPoint -> Just Refl
    (,) CList   CList  -> Just Refl
    (,) CSet    CSet   -> Just Refl
    (,) CTree   CTree  -> Just Refl
    (,) CDag    CDag   -> Just Refl
    (,) CGraph  CGraph -> Just Refl
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
       CPoint -> 0
       CList  -> 1
       CSet   -> 2
       CTree  -> 3
       CDag   -> 4
       CGraph -> 5

instance ReifyCTag Point where reifyCTag = const CPoint
instance ReifyCTag List  where reifyCTag = const CList
instance ReifyCTag 'Set  where reifyCTag = const CSet
instance ReifyCTag Tree  where reifyCTag = const CTree
instance ReifyCTag Dag   where reifyCTag = const CDag
instance ReifyCTag Graph where reifyCTag = const CGraph

instance Serialise SomeCTag where
  encode = CBOR.encodeWord . \(SomeCTag tag) -> case tag of
    CPoint -> 1
    CList  -> 2
    CSet   -> 3
    CTree  -> 4
    CDag   -> 5
    CGraph -> 6
  decode = do
    tag <- CBOR.decodeWord
    case tag of
      1 -> pure $ SomeCTag CPoint
      2 -> pure $ SomeCTag CList
      3 -> pure $ SomeCTag CSet
      4 -> pure $ SomeCTag CTree
      5 -> pure $ SomeCTag CDag
      6 -> pure $ SomeCTag CGraph
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
mapRepr CPoint f = f
mapRepr CList  f = fmap f
mapRepr CSet   f = fmap f
mapRepr CTree  f = fmap f
mapRepr CDag   f = fmap f
mapRepr CGraph f = fmap f

--------------------------------------------------------------------------------
-- * Utilities
--
withCTag :: CTag c -> ((Typeable c, ReifyCTag c) => r) -> r
withCTag c f = case c of
  CPoint -> f
  CList  -> f
  CSet   -> f
  CTree  -> f
  CDag   -> f
  CGraph -> f

parseCTag
  :: forall m
  . (MonadFail m, TokenParsing m)
  => m (Some CTag)
parseCTag = do
  i <- ctagIdentifier
  case i of
    "Point" -> pure $ Exists CPoint
    "List"  -> pure $ Exists CList
    "Set"   -> pure $ Exists CSet
    "Tree"  -> pure $ Exists CTree
    "Dag"   -> pure $ Exists CDag
    "Graph" -> pure $ Exists CGraph
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
