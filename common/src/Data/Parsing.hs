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

type Parser a = ParsecT Text Text Identity a

-- * From the old Ground.Parser:
--
-- import Control.Monad.Fail (MonadFail)
-- import Data.IntervalMap.FingerTree (Interval(..))
-- import Text.Parser.Combinators ((<?>), try)
-- import Text.Parser.Char (alphaNum, char, letter, string)
-- import Text.Parser.Token.Highlight
--        (Highlight(..))
-- import Text.Parser.Token
--        (IdentifierStyle(..), TokenParsing
--        , ident, token)
