module Dom.Parse (module Dom.Parse) where

import           Data.Typeable                      (Proxy(..), Typeable)
import           Type.Reflection                    (someTypeRep)

import Data.Parsing


--------------------------------------------------------------------------------
-- * Generic parser
--
class Parse a where
  -- parser :: (MonadParsec Text Text m, TokenParsing m) => m a
  parser :: Parser a

instance {-# OVERLAPPABLE #-} Typeable a => Parse a where
  parser = failParser

failParser :: forall m a. (MonadFail m, TokenParsing m, Typeable a) => m a
failParser = fail ("No parser for " <> show (someTypeRep $ Proxy @a) <> ".")
