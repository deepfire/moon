{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Pipe.PipeSpace
  ( SomePipeSpace
  , PipeSpace(..)
  , SomePipeScope
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

instance Functor PipeSpace where
  fmap f ps@PipeSpace{psSpace} =
    ps { psSpace = f <$> psSpace }

--------------------------------------------------------------------------------
-- * PipeSpace -- move to Pipe.Space?
--

instance (Eq a, Serialise a, Typeable a)
 => Serialise (PipeSpace a) where
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
      (5, 2177) -> do
        name <- decode
        spc <- decode
        PipeSpace
          <$> pure name
          <*> pure spc
          <*> (MMap.map Set.fromList <$> decode)
          <*> (MMap.map Set.fromList <$> decode)
      _ -> fail $ "PipeSpace failed decode: "
           <>" len="<> show len
           <>" tag="<> show tag
           <>" rep="<> show (typeRep @(PipeSpace a))

instance Semigroup (PipeSpace a) where
  l <> r = PipeSpace
    { psName  = psName  l
    , psSpace = psSpace l <> psSpace r
    , psFrom  = psFrom  l <> psFrom  r
    , psTo    = psTo    l <> psTo    r
    }

instance Monoid (PipeSpace a) where
  mempty = PipeSpace mempty mempty mempty mempty

type SomePipeScope p = PointScope (SomePipe p)

instance Typeable a => Read (PipeSpace a) where readPrec = failRead

instance Show (PipeSpace a) where
  show PipeSpace{psName, psSpace} =
    "(PipeSpace "<>show psName<>" "<>show (length $ spaceEntries psSpace)<>" entries)"
