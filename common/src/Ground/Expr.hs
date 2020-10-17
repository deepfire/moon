{-# LANGUAGE OverloadedStrings #-}
module Ground.Expr
  ( parseGroundExpr
  )
where

import Control.Applicative (some)
import Control.Monad (foldM)
import qualified Data.IntervalMap.FingerTree            as IMap

import Text.Megaparsec (runParserT, eof)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Parsers (unParsecT, sepBy1)
import Text.Parser.Token

import Data.Parsing
import Basis

import Dom.Expr

import Ground.Parser (parseQName', holeToken) -- No Ground needed.
import Ground.Table (parseExpr)

import Pipe.Types



parseGroundExpr :: Text -> Either Text (Expr (Located (QName Pipe)))
parseGroundExpr = parse parseQName'

parse
  :: forall e n. (e ~ Text)
  => (Bool -> Parser n)
  -> Text
  -> Either e (Expr n)
parse nameParser = tryParse True
 where
   tryParse :: Bool -> Text -> Either e (Expr n)
   tryParse mayExtend s =
     case (,)
          (runIdentity $ runParserT
           (do
               x <- unParsecT $ parseExpr (nameParser (not mayExtend))
               Text.Megaparsec.eof
               pure x)
            "" s)
          mayExtend
     of
       -- If parse succeeds, then propagate immediately.
       (Right (Right x), _)     -> Right x
       -- If we don't parse, and we can't extend, then fail immediately.
       (Left         e,  False) -> Left $ "Pipe expr parser: " <> pack (show e)
       (Right (Left  e), False) -> Left $ pack (show e)
       -- If we don't parse, and we can extend, then retry with extension allowed.
       (_,               True)  -> tryParse False (s <> holeToken)
