module Dom.Tags (module Dom.Tags) where


import           Control.DeepSeq                    (NFData(..))
import           Data.Typeable                      (Proxy(..), Typeable)

import Dom.CTag
import Dom.VTag


--------------------------------------------------------------------------------
-- * Tags:  Constructor * Value
--
data Tags ty where
  Tags :: (ty ~ CTagV c a, ReifyCTag c, ReifyVTag a, Typeable c) =>
    { tCTag :: CTag c
    , tVTag :: VTag a
    } -> Tags (CTagV c a)
  deriving Typeable

instance NFData (Tags a) where
  rnf _ = ()

typesTags ::
  forall c a. (ReifyCTag c, ReifyVTag a)
  => CTagV c a -> Tags (CTagV c a)
typesTags _ =
  withCTag ctag $
    Tags ctag (reifyVTag $ Proxy @a)
 where ctag = reifyCTag $ Proxy @c
