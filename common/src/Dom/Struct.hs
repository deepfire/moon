{-# LANGUAGE UndecidableInstances       #-}
module Dom.Struct (module Dom.Struct) where

import qualified Algebra.Graph                    as G
import           Codec.Serialise                    (Serialise)
import           Control.DeepSeq                    (NFData)
import           GHC.Generics                       (Generic)
import           Text.Read                          (Read(..))

import Data.Orphanage

import Dom.SomeType


--------------------------------------------------------------------------------
-- * Struct: Pipe's internal structure,
--   as a graph of type components.
--
newtype Struct =
  Struct (G.Graph SomeType)
  deriving (Eq, Generic, Semigroup, Monoid, Ord, Show)

instance Semigroup (G.Graph a) where
  (<>) = G.overlay

instance Monoid (G.Graph a) where
  mempty = G.empty

--------------------------------------------------------------------------------
-- * Instances
--
instance NFData Struct

instance Serialise Struct
instance Read Struct where readPrec = failRead
