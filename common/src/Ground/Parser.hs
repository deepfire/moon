{-# OPTIONS_GHC -Wno-orphans #-}
module Ground.Parser
  ( parseCTag
  , parseQName
  , parseQName'
  , parseName
  , parseName'
  , holeToken
  )
where

import Control.Monad.Fail (MonadFail)
import Data.IntervalMap.FingerTree (Interval(..))
import Text.Parser.Combinators ((<?>), try)
import Text.Parser.Char (alphaNum, char, letter, string)
import Text.Parser.Token.Highlight
       (Highlight(..))
import Text.Parser.Token
       (IdentifierStyle(..), TokenParsing
       , ident, token)

import Basis
import Data.Parsing
import Data.Some
import Type


-- * Some CTag
--
instance Parse (Some CTag) where
  parser = parseCTag

parseCTag
  :: forall m
  . (MonadFail m, TokenParsing m)
  => m (Some CTag)
parseCTag = do
  i <- ctagIdentifier
  case i of
    "Point" -> pure $ Exists TPoint
    "List"  -> pure $ Exists TList
    "Set"   -> pure $ Exists TSet
    "Tree"  -> pure $ Exists TTree
    "Dag"   -> pure $ Exists TDag
    "Graph" -> pure $ Exists TGraph
    x -> fail $ "Mal-Con: " <> show x

ctagIdentifier :: (Monad m, TokenParsing m) => m Text
ctagIdentifier = ident $ IdentifierStyle
  { _styleName = "Con tag"
  , _styleStart = letter
  , _styleLetter = alphaNum
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }


-- * QName
--
instance Typeable a => Parse (QName a) where
  parser = parseQName

-- An internal token that serves as a hint to allow the expression to be extended openly.
holeToken :: Text
holeToken = "!"

parseQName
  :: forall e a
  . (e ~ Text)
  => ParsecT e Text Identity (QName a)
parseQName = locVal <$> parseQName' False

parseQName'
  :: forall e a. (e ~ Text)
  => Bool -> ParsecT Text Text Identity (Located (QName a))
parseQName' allowHole =
  if allowHole
  then doParse tok <|> doParse (QName mempty
                                 <$ string (unpack holeToken))
  else doParse tok
 where
   tok = textQName <$> alnumTokenDotty
   doParse x = token $ do
     pre <- getOffset
     v <- x
     flip Locn v . Interval pre <$> getOffset
   alnumTokenDotty :: (TokenParsing m, Monad m) => m Text
   alnumTokenDotty = fmap pack . try $
     (:) <$> alphaNum <*> many constituent
       where constituent = alphaNum <|> char '.'


-- * Name
--
instance Typeable a => Parse (Name a) where
  parser = parseName

parseName
  :: forall e m a
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => m (Name a)
parseName = parseName' False

parseName'
  :: forall e m a
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => Bool -> m (Name a)
parseName' allowHole =
  Name <$> (if allowHole
            then nameIdentifier <|> pure holeToken
            else nameIdentifier)
  <?> "Name"

nameIdentifier :: (Monad m, TokenParsing m) => m Text
nameIdentifier = ident $ IdentifierStyle
  { _styleName = "Name"
  , _styleStart = alphaNum
  , _styleLetter = alphaNum <|> char '.'
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
