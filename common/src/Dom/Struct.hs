{-# LANGUAGE UndecidableInstances       #-}
module Dom.Struct (module Dom.Struct) where

import qualified Algebra.Graph                    as G
import           Codec.Serialise                    (Serialise)
import           Control.DeepSeq                    (NFData)
import           Generics.SOP (I(..))
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
  deriving (Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------
-- * Instances
--
instance NFData Struct

instance Serialise Struct
instance Read Struct where readPrec = failRead
