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
import Dom.CTag
import Dom.Located
import Dom.Name
import Dom.Parse
import Dom.Some

