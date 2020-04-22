module Ground.Parser
  ( parseTag
  , parseQName
  , parseQName'
  , parseName
  , parseName'
  , magicToken
  )
where

import Text.Parser.Combinators ((<?>), try)
import Text.Parser.Char (alphaNum, char, letter, string)
import Text.Parser.Token.Highlight
       (Highlight(..))
import Text.Parser.Token
       (IdentifierStyle(..), TokenParsing
       , ident, token, someSpace)

import Basis
import Data.Parsing
import Data.Some
import Type


-- * Some Tag
--
instance Parse (Some Tag) where
  parser = parseTag

parseTag
  :: forall m
  . (MonadFail m, TokenParsing m)
  => m (Some Tag)
parseTag = do
  i <- tagIdentifier
  case i of
    "Point" -> pure $ Exists TPoint
    "List"  -> pure $ Exists TList
    "Set"   -> pure $ Exists TSet
    "Tree"  -> pure $ Exists TTree
    "Dag"   -> pure $ Exists TDag
    "Graph" -> pure $ Exists TGraph
    x -> fail $ "Mal-Con: " <> show x

tagIdentifier :: (Monad m, TokenParsing m) => m Text
tagIdentifier = ident $ IdentifierStyle
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

magicToken :: Text
magicToken = "!"

parseQName
  :: forall e a
  . (e ~ Text)
  => ParsecT e Text Identity (QName a)
parseQName = snd3 <$> parseQName' False

parseQName'
  :: forall e a. (e ~ Text)
  => Bool -> ParsecT Text Text Identity (Int, QName a, Int)
parseQName' allowMagic =
  if allowMagic
  then doParse tok <|> doParse ((QName mempty)
                                <$ string (unpack magicToken))
  else doParse tok
 where
   tok = textQName <$> alnumTokenDotty
   doParse x = token $ do
     pre <- getOffset
     v <- x
     post <- getOffset
     -- someSpace
     -- (someSpace <|> pure ())
     pure (pre, v, post)
   alnumTokenDotty :: (TokenParsing m, Monad m) => m Text
   alnumTokenDotty = fmap pack $ try $ do
     name <- ((:) <$> alphaNum <*> many constituent)
     return name
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
parseName' allowMagic =
  Name <$> (if allowMagic
            then nameIdentifier <|> pure magicToken
            else nameIdentifier)
  <?> "Name"
 where

nameIdentifier :: (Monad m, TokenParsing m) => m Text
nameIdentifier = ident $ IdentifierStyle
  { _styleName = "Name"
  , _styleStart = alphaNum
  , _styleLetter = alphaNum <|> char '.'
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
