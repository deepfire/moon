module Dom.Parse (module Dom.Parse) where

import           Control.Monad.Fail                 (MonadFail)
import           Data.Typeable                      (Proxy(..), Typeable, (:~:)(..), (:~~:)(..))
import           Type.Reflection                    (someTypeRep)

import Data.Parsing
import Dom.CTag


--------------------------------------------------------------------------------
-- * Generic parser
--
class Parse a where
  -- parser :: (MonadParsec Text Text m, TokenParsing m) => m a
  parser :: Parser a

instance {-# OVERLAPPABLE #-} Typeable a => Parse a where
  parser = failParser
    where
      failParser :: (MonadFail m, TokenParsing m) => m a
      failParser = fail ("No parser for " <> show (someTypeRep $ Proxy @a) <> ".")
