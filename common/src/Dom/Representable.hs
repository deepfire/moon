module Dom.Representable (module Dom.Representable) where

import qualified Codec.CBOR.Decoding              as CBOR (decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (encodeWord)
import           Codec.Serialise                    (Serialise(..))
import           Control.DeepSeq                    (NFData(..))
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import           Data.Typeable                      (Proxy, Typeable, (:~:)(..), (:~~:)(..))
import           GHC.Generics                       (Generic)


--------------------------------------------------------------------------------
-- * Representable
--
class Representable (a :: *) where
  type Present a :: *
  repr :: a -> Present a

instance {-# OVERLAPPABLE #-} Representable a where
  type Present a = a
  repr = id
