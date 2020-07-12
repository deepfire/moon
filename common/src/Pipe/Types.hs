{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-orphans -Wno-unticked-promoted-constructors #-}
module Pipe.Types
  ( SomePipeSpace
  , PipeSpace(..)
  , SomePipeScope
  , Ops(..)
  , PipeOps(..)
  , Value(..)

  -- reexports
  , module Pipe.Pipe
  , module Pipe.SomePipe
  , module SomeType
  , module SomeValue
 )
where

import           Codec.Serialise
import           Codec.CBOR.Encoding                (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (decodeListLen, decodeWord)
import           Data.Dynamic
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set

import Basis
import Type
import SomeType
import SomeValue
import Namespace (Space, PointScope, spaceEntries)
import Pipe.Pipe
import Pipe.SomePipe

--------------------------------------------------------------------------------
-- * Key types
--
type SomePipeSpace p = PipeSpace (SomePipe p)

data PipeSpace a = PipeSpace
  { psName  :: !(QName PipeSpace)
  , psSpace :: !(Space Point a)
  , psFrom  :: !(MonoidalMap (Maybe SomeTypeRep) (Set (QName Pipe)))
  , psTo    :: !(MonoidalMap        SomeTypeRep  (Set (QName Pipe)))
  } deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- * PipeSpace -- move to Pipe.Space?
--

instance (Ord a, Serialise a, Typeable a) => Serialise (PipeSpace a) where
  encode PipeSpace{psName, psSpace, psFrom, psTo} =
    encodeListLen 5
    <> encodeWord 2177
    <> encode psName
    <> encode psSpace
    <> encode (MMap.map setToList psFrom)
    <> encode (MMap.map setToList psTo)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (5, 2177) ->
        PipeSpace
        <$> decode
        <*> decode
        <*> (MMap.map Set.fromList <$> decode)
        <*> (MMap.map Set.fromList <$> decode)
      _ -> fail $ "PipeSpace failed decode: "
           <>" len="<> show len
           <>" tag="<> show tag
           <>" rep="<> show (typeRep @(PipeSpace a))

instance Semigroup (PipeSpace a) where
  l <> r = PipeSpace
    { psName  = psName  l <> psName  r
    , psSpace = psSpace l <> psSpace r
    , psFrom  = psFrom  l <> psFrom  r
    , psTo    = psTo    l <> psTo    r
    }

type SomePipeScope p = PointScope (SomePipe p)

--------------------------------------------------------------------------------
-- * Operations
--
data Ops p where
  Ops ::
    { app
      :: forall c kas kas' o ka
      . ( PipeConstr c kas  o
        , PipeConstr c kas' o
        , kas ~ (ka : kas')
        )
      => Desc c kas o -> Value (TagOf ka) (TypeOf ka) -> p -> Either Text p
    , comp
      :: forall cf cv vas vo fas fass ras fo
      . ( PipeConstr cv vas vo
        , PipeConstr cf fas fo
        , fas ~ (vo:fass)
        , ras ~ fass
        )
      => Desc cv vas vo -> p -> Desc cf fas fo -> p -> Either Text p
    , trav
      :: forall cf ct fas fo a tas to
      . ( PipeConstr cf fas fo
        , PipeConstr ct tas to
        , fas ~ (Type Point a ': '[])
        , tas ~ '[]
        , TypeOf to ~ a
        , TagOf fo ~ 'Point)
      => Desc cf fas fo -> p -> Desc ct tas to -> p -> Either Text p
    } -> Ops p

-- This allows pipe operations (apply, compose, traverse) to be
-- performed over the HKDT.
class PipeOps p where
  pipeOps   :: Ops p

--------------------------------------------------------------------------------
-- * Important instances
--
instance Functor PipeSpace where
  fmap f ps@PipeSpace{psSpace} =
    ps { psSpace = f <$> psSpace }

{-------------------------------------------------------------------------------
  Really boring.
-------------------------------------------------------------------------------}
instance Typeable a => Read (PipeSpace a) where readPrec = failRead

instance Show (PipeSpace a) where
  show PipeSpace{psName, psSpace} =
    "(PipeSpace "<>show psName<>" "<>show (length $ spaceEntries psSpace)<>" entries)"
