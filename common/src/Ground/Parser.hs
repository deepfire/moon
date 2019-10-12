module Ground.Parser
  ( parseTag
  , parseQName
  , parseName
  )
where

import           Control.Applicative                ((<|>))
import qualified Data.Sequence                    as Seq
import           Data.Text                          (split)

import Text.Parser.Combinators ((<?>))
import Text.Parser.Char (alphaNum, char, letter, string)
import Text.Parser.Token.Highlight
       (Highlight(..))
import Text.Parser.Token
       (IdentifierStyle(..), TokenParsing
       , ident)

import Basis
import Data.Some
import Type


-- * Some Tag
--
-- instance Parser (Some Tag) where
--   parser = parseTag

parseTag
  :: forall m
  . (Monad m, TokenParsing m)
  => m (Some Tag)
parseTag = do
  i <- identifier
  case i of
    "Point" -> pure $ Exists TPoint
    "List"  -> pure $ Exists TList
    "Set"   -> pure $ Exists TSet
    "Tree"  -> pure $ Exists TTree
    "Dag"   -> pure $ Exists TDag
    "Graph" -> pure $ Exists TGraph
    x -> fail $ "Mal-Con: " <> show x
 where
   identifier :: (Monad m, TokenParsing m) => m Text
   identifier = ident $ IdentifierStyle
     { _styleName = "Con tag"
     , _styleStart = letter
     , _styleLetter = alphaNum
     , _styleReserved = mempty
     , _styleHighlight = Identifier
     , _styleReservedHighlight = ReservedIdentifier
     }


-- * QName
--
instance Typeable (a :: *) => Parser (QName a) where
  parser = parseQName

parseQName
  :: forall e m a
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => m (QName a)
parseQName =
  (string "()" >> pure (QName mempty))
  <|>
  (QName . (Name <$>) . Seq.fromList . split (== '.') <$> identifier)
  <?> "QName"
 where
  identifier :: (Monad m, TokenParsing m) => m Text
  identifier = ident $ IdentifierStyle
    { _styleName = "QName"
    , _styleStart = alphaNum
    , _styleLetter = alphaNum <|> char '.'
    , _styleReserved = mempty
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }


-- * Name
--
instance Typeable (a :: *) => Parser (Name a) where
  parser = parseName

parseName
  :: forall e m a
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => m (Name a)
parseName =
  Name <$> identifier
  <?> "Name"
 where
  identifier :: (Monad m, TokenParsing m) => m Text
  identifier = ident $ IdentifierStyle
    { _styleName = "Name"
    , _styleStart = alphaNum
    , _styleLetter = alphaNum <|> char '.'
    , _styleReserved = mempty
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }
