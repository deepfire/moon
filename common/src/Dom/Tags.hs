module Dom.Tags (module Dom.Tags) where


import qualified Codec.CBOR.Decoding              as CBOR (decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (encodeWord)
import           Codec.Serialise                    (Serialise(..))
import           Control.DeepSeq                    (NFData(..))
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import           Data.Typeable                      (Proxy(..), Typeable, (:~:)(..), (:~~:)(..))
import           GHC.Generics                       (Generic)

import Dom.CTag
import Dom.VTag


--------------------------------------------------------------------------------
-- * Tags:  Constructor * Value
--
data Tags ty where
  Tags :: (ty ~ Types c a, ReifyCTag c, ReifyVTag a, Typeable c, Typeable a) =>
    { tCTag :: CTag c
    , tVTag :: VTag a
    } -> Tags (Types c a)
  deriving Typeable

instance NFData (Tags a) where
  rnf _ = ()

typesTags ::
  forall c a. (ReifyCTag c, ReifyVTag a, Typeable c, Typeable a)
  => Types c a -> Tags (Types c a)
typesTags _ = Tags (reifyCTag $ Proxy @c) (reifyVTag $ Proxy @a)



