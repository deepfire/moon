{-# LANGUAGE UndecidableInstances       #-}
module Dom.Sig (module Dom.Sig) where

import           Codec.Serialise                    (Serialise)
import           Control.DeepSeq                    (NFData)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Generics.SOP                       (I(..), unI)
import           Text.Read                          (Read(..))
import           Type.Reflection                    (Typeable)

import qualified Data.Text       as T

import Data.Orphanage

import Dom.SomeType


--------------------------------------------------------------------------------
-- * Sig:  serialisable type signature
--
type ISig = Sig I
type MSig = Sig Maybe

data Sig f =
  Sig
  { sArgs :: [f SomeType]
  , sOut  :: f SomeType
  }
  deriving (Generic)

newtype ListSig f = ListSig { unListSig :: [f SomeType] }

--------------------------------------------------------------------------------
-- * Instances
--
deriving instance (Eq  (f SomeType)) => Eq (Sig f)
deriving instance (Ord (f SomeType)) => Ord (Sig f)
instance NFData (f SomeType) => NFData (Sig f)
instance Show ISig where
  show x  =  "("<>T.unpack (showSig x)<>")"
instance (Serialise (f SomeType)) => Serialise (Sig f)
instance Typeable f => Read (Sig f) where readPrec = failRead

--------------------------------------------------------------------------------
-- * Utils
--
toListSig :: Sig f -> ListSig f
toListSig Sig{..} = ListSig $ sArgs <> [sOut]

fromListSig :: ListSig f -> Maybe (Sig f)
fromListSig = \case
  ListSig [] -> Nothing
  ListSig xs -> Just $ Sig (init xs) (last xs)

showSig :: ISig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSig (Sig as o) = T.intercalate " → " $ showSomeType False . unI <$> (as <> [o])

showSigDotty :: ISig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSigDotty (Sig as o) = T.intercalate " → " $ showSomeType True . unI <$> (as <> [o])
