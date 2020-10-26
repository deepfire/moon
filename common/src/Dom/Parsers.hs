module Dom.Parsers (module Dom.Parsers) where

import          Data.Typeable (Typeable)

import Dom.CTag
import Dom.Name
import Dom.Parse
import Dom.Some



-- * Bind them all..
--
instance Parse (Some CTag) where
  parser = parseCTag

instance Typeable a => Parse (QName a) where
  parser = parseQName

instance Typeable a => Parse (Name a) where
  parser = parseName
