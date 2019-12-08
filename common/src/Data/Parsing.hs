module Data.Parsing
  ( Parser
  , module Control.Applicative
  , module Data.Functor.Identity
  , module Text.Megaparsec
  , module Text.Megaparsec.Parsers
  , module Text.Megaparsec.Pos
  )
where

import           Control.Applicative                ((<|>), many)
import           Data.Functor.Identity              (Identity(..))
import           Data.Text                          (Text)
import           Text.Megaparsec                    (MonadParsec, getOffset)
import           Text.Megaparsec.Parsers            (ParsecT)
import           Text.Megaparsec.Pos

type Parser a = ParsecT Text Text Identity a
