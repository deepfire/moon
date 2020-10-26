module Basis
  ( module Codec.CBOR.Encoding
  , module Codec.CBOR.Decoding
  , module Codec.Serialise
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Concurrent.STM
  , module Control.DeepSeq
  , module Control.Monad
  , module Control.Monad.Fail
  , module Control.Tracer
  , module Data.Bifunctor
  , module Data.Bifunctor.Swap
  -- , module Data.Coerce
  , module Data.Dict
  , module Data.Dynamic
  , module Data.Either
  , module Data.Either.Combinators
  , module Data.Either.Extra
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.GADT.Compare
  , module Data.Hashable
  , module Data.HashMap.Strict
  , module Data.Kind
  , module Data.List
  , module Data.List.Extra
  , module Data.List.NonEmpty
  , module Data.Map.Monoidal.Strict
  , module Data.Map.Strict
  , module Data.Maybe
  , module Data.Orphanage
  , module Data.Proxy
  , module Data.Sequence
  , module Data.Set.Monad
  , module Data.SOP.Dict
  , module Data.SOP.Constraint
  , module Data.String
  , module Data.Text
  , module Data.Tuple.Extra
  , module Data.Type.Equality
  , module Data.Type.List
  , module Data.TypeRep
  , module Data.Witherable
  , module Debug.Trace
  , module Debug.TraceErr
  , module Generics.SOP
  , module Text.Printf
  , module Text.Read
  , module Type.Reflection
  -- * Locals
  , module Basis
  )
where

import Codec.Serialise            (Serialise(..))
import Codec.CBOR.Encoding        (Encoding, encodeListLen, encodeWord)
import Codec.CBOR.Decoding        (Decoder, decodeListLen, decodeWord)
import Control.Applicative        ((<|>), liftA2)
import Control.Arrow              ((>>>), (***), (&&&), (+++), left, right, first, second)
import Control.Concurrent.STM     (STM, atomically)
import Control.DeepSeq            (NFData(..))
import Control.Monad              (foldM, join, mapM, mapM_, forM, forM_, void)
import Control.Monad.Fail         (MonadFail)
import Control.Tracer             (Tracer(..), traceWith)
import Data.Bifunctor             (bimap)
import Data.Bifunctor.Swap        (swap)
import Data.Dict                  (TyDict(..), TyDicts)
import Data.Dynamic               (Dynamic(..), Typeable)
import Data.Either                (partitionEithers)
import Data.Either.Combinators    (maybeToLeft, maybeToRight)
import Data.Either.Extra          (mapLeft, mapRight, fromLeft, fromRight, eitherToMaybe, maybeToEither)
import Data.Function              ((&), on)
import Data.Functor               ((<&>), (<$), ($>))
import Data.Foldable              (toList)
import Data.GADT.Compare          (GOrdering(..))
import Data.Hashable              (Hashable)
import Data.HashMap.Strict        (HashMap)
import Data.Kind                  (Constraint)
import Data.List                  (sortBy)
import Data.List.Extra            (unsnoc)
import Data.List.NonEmpty         (NonEmpty(..), (<|))
import Data.Map.Strict            (Map)
import Data.Maybe                 (isJust, isNothing, fromMaybe)
import Data.Map.Monoidal.Strict   (MonoidalMap)
import Data.Orphanage
import Data.Proxy                 (Proxy(..))
import Data.Sequence              (Seq)
import Data.Set.Monad             (Set)
import Data.SOP.Dict              (Dict(..))
import Data.SOP.Constraint        (Head, Tail)
import Data.String                (IsString)
import Data.Text                  (Text, pack, unpack)
import Data.Tuple.Extra           (fst3, snd3, thd3, uncurry3)
import Data.Type.Equality         ((:~:)(..), (:~~:)(..))
import Data.Type.List             (spineConstraint)
import Data.TypeRep               (showSomeTypeRep, showTypeRep)
import Data.Witherable            (catMaybes, mapMaybe, wither)
import Debug.Trace                (trace, traceM)
import Debug.TraceErr             (traceErr, traceIOErr)
import Generics.SOP               (All, All2, And, Compose, Code, NP(..), NS, Top
                                  , I(..), unI, K(..), unK)
import Text.Printf                (printf)
import Text.Read                  (Read(..))
import Type.Reflection            ((:~:)(..), (:~~:)(..),
                                   TypeRep, SomeTypeRep(..),
                                   eqTypeRep, someTypeRep, typeRep, typeRepKind, withTypeable)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Set.Monad  as Set
import qualified Data.Set        as Set'
import qualified Data.Text       as T
import qualified GHC.Types       as GHC
import qualified Text.Builder    as TB
import qualified Type.Reflection as Refl


liftSet :: Ord a => Set'.Set a -> Set.Set a
liftSet = Set.fromDistinctAscList . Set'.toAscList

unliftSet :: Ord a => Set.Set a -> Set'.Set a
unliftSet = Set'.fromDistinctAscList . Set.toAscList

keysSet :: Ord k => Map k v -> Set k
keysSet = liftSet . Map.keysSet

keysSetMMap :: Ord k => MonoidalMap k v -> Set k
keysSetMMap = liftSet . MMap.keysSet

listSetUnsafe :: Ord a => [a] -> Set.Set a
listSetUnsafe = Set.fromDistinctAscList

setToList :: Ord a => Set.Set a -> [a]
setToList = Set.toAscList

trc :: (a -> String) -> a -> a
trc f x = trace (f x) x

dynRep :: Dynamic -> SomeTypeRep
dynRep (Dynamic rep _) = SomeTypeRep rep

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

eitherLeft :: Either a b -> Maybe a
eitherLeft = \case
  Left a  -> Just a
  Right{} -> Nothing

eitherRight :: Either a b -> Maybe b
eitherRight = eitherToMaybe

maybeLeft :: (a -> Maybe b) -> a -> Either b a
maybeLeft guard x =
  case guard x of
    Just e -> Left e
    Nothing -> Right x

fst4 :: (,,,) a b c d -> a
fst4 (x, _, _, _) = x

snd4 :: (,,,) a b c d -> b
snd4 (_, x, _, _) = x

thd4 :: (,,,) a b c d -> c
thd4 (_, _, x, _) = x

fth4 :: (,,,) a b c d -> d
fth4 (_, _, _, x) = x

fst5 :: (,,,,) a b c d e -> a
fst5 (x, _, _, _, _) = x

snd5 :: (,,,,) a b c d e -> b
snd5 (_, x, _, _, _) = x

thd5 :: (,,,,) a b c d e -> c
thd5 (_, _, x, _, _) = x

frt5 :: (,,,,) a b c d e -> d
frt5 (_, _, _, x, _) = x

fif5 :: (,,,,) a b c d e -> e
fif5 (_, _, _, _, x) = x

fst6 :: (,,,,,) a b c d e f -> a
fst6 (x, _, _, _, _, _) = x

snd6 :: (,,,,,) a b c d e f -> b
snd6 (_, x, _, _, _, _) = x

thd6 :: (,,,,,) a b c d e f -> c
thd6 (_, _, x, _, _, _) = x

frt6 :: (,,,,,) a b c d e f -> d
frt6 (_, _, _, x, _, _) = x

fif6 :: (,,,,,) x b c d e f -> e
fif6 (_, _, _, _, x, _) = x

sxt6 :: (,,,,,) a b c d e f -> f
sxt6 (_, _, _, _, _, x) = x

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f <$$> xs = (f <$>) <$> xs

(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)

rcons2 :: (a, b) -> c -> (a, b, c)
rcons2 (a, b) c = (a, b, c)

lcons2 :: a -> (b, c) -> (a, b, c)
lcons2 a (b, c) = (a, b, c)

rcons3 :: (a, b, c) -> d -> (a, b, c, d)
rcons3 (a, b, c) d = (a, b, c, d)

lcons3 :: a -> (b, c, d) -> (a, b, c, d)
lcons3 a (b, c, d) = (a, b, c, d)

luncons3 :: (a, b, c) -> (a, (b, c))
luncons3 (a, b, c) = (a, (b, c))

rpop3 :: (a, b, c) -> (a, b)
rpop3 (a, b, _) = (a, b)

listTyCon, tuple2TyCon, tuple3TyCon, charTyCon :: GHC.TyCon
listTyCon   = Refl.typeRepTyCon $ typeRep @[()]
tuple2TyCon = Refl.typeRepTyCon $ typeRep @((),())
tuple3TyCon = Refl.typeRepTyCon $ typeRep @((),(),())
charTyCon   = Refl.typeRepTyCon $ typeRep @Char

stderr :: Tracer IO Text
stderr = Tracer $ traceIOErr . T.unpack
