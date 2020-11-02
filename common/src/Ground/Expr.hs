{-# LANGUAGE OverloadedStrings #-}
module Ground.Expr (module Ground.Expr) where

import Control.Applicative (some)
import Control.Monad (foldM)
import qualified Data.IntervalMap.FingerTree            as IMap
import Data.String (IsString(..))

import Text.Megaparsec (runParserT, eof)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Parsers (unParsecT, sepBy1)
import Text.Parser.Token

import Basis
import Data.Parsing

import Dom.Command
import Dom.Expr
import Dom.Error
import Dom.Located
import Dom.Name
import Dom.Pipe

import Ground.Table (parseSomeValueLiteral, someValueText)



-- * Ground table-dependent parsing (due to ground literals)
--
parseGroundExpr :: Text -> Fallible (Expr (Located (QName Pipe)))
parseGroundExpr = _parse parseQName' (Dom.Expr.parseExpr
                                      parseSomeValueLiteral)

parseGroundCommand :: Text -> Fallible (Command (Located (QName Pipe)))
parseGroundCommand = _parse parseQName' (Dom.Command.parseCommand
                                         parseSomeValueLiteral)

instance IsString (Expr (Located (QName Pipe))) where
  fromString = either err id . parseGroundExpr . pack
    where err = PVal . someValueText . ("parse error: " <>) . showError

_parse
  :: forall (f :: * -> *) (n :: *)
  .  (Bool -> Parser n)
  -> (Parser n -> Parser (Either Text (f n)))
  -> Text
  -> Fallible (f n)
_parse nameParser parser = tryParse True
 where
   tryParse :: Bool -> Text -> Fallible (f n)
   tryParse mayExtend s =
     case (,)
          (runIdentity $ runParserT
           (do
               x <- unParsecT $
                 parser (nameParser (not mayExtend))
               Text.Megaparsec.eof
               pure x)
            "" s)
          mayExtend
     of
       -- If parse succeeds, then propagate immediately.
       (Right (Right x), _)     -> Right x
       -- If we don't parse, and we can't extend, then fail immediately.
       (Left         e,  False) -> fallDescShow "Pipe expr parser" e
       (Right (Left  e), False) -> fallShow e
       -- If we don't parse, and we can extend, then retry with extension allowed.
       (_,               True)  -> tryParse False (s <> holeToken)
