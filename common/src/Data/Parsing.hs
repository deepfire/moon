module Data.Parsing
  ( Parser
  , module Control.Applicative
  , module Control.Monad.Fail
  , module Data.Functor.Identity
  , module Text.Megaparsec
  , module Text.Megaparsec.Parsers
  , module Text.Megaparsec.Pos
  , module Text.Parser.Token
  , module Text.Parser.Token.Highlight
  , module Text.Read
  )
where

import           Control.Applicative                ((<|>), many)
import           Control.Monad.Fail                 (MonadFail)
import           Data.Functor.Identity              (Identity(..))
import           Data.Text                          (Text)
import           Text.Megaparsec                    (MonadParsec, getOffset)
-- import           Text.Megaparsec.Char               (string)
import           Text.Megaparsec.Parsers
import           Text.Megaparsec.Pos
import           Text.Parser.Token
                   (IdentifierStyle(..), TokenParsing, ident, token)
import           Text.Parser.Token.Highlight        (Highlight(..))
import           Text.Read                          (Lexeme(Ident), Read(..), lexP)
import           Data.Void                          (Void)

type Parser a = ParsecT Void Text Identity a
