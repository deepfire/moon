module Dom.Located
  ( module Dom.Located
  , Interval
  )
where

import           Codec.Serialise                    (Serialise)
import           Data.IntervalMap.FingerTree        (Interval(..))
import           GHC.Generics                       (Generic)

import Data.Orphanage


--------------------------------------------------------------------------------
-- * Generalised location
--
-- TODO:  consider using a single Loc/Located type
data Located a
  = Locn
    { locSpan :: {-# UNPACK #-} !(Interval Int)
    , locVal  :: !a
    }
  deriving (Generic, Functor)

instance Serialise a => Serialise (Located a)

instance Show a => Show (Located a) where
  show = show . locVal
