{-# LANGUAGE OverloadedStrings #-}
module Ground.Expr (module Ground.Expr) where

import Data.String (IsString(..))
import qualified Data.Text as T

import Text.Megaparsec (runParserT, eof)
import Text.Megaparsec.Error

import Basis
import Data.Parsing

import Dom.Expr
import Dom.Error
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe

import Ground.Table (parseSomeValueLiteral, someValueText)



-- * Ground dependents
--
instance IsString (Expr (Located (QName Pipe))) where
  fromString = either err id . parseGroundExpr Nothing . pack
    where err = PVal . someValueText . ("parse error: " <>) . showError . unEPipe


-- * Ground table-dependent parsing (due to ground literals)
--
parseGroundExpr :: Maybe Int -> Text -> PFallible (Expr (Located (QName Pipe)))
parseGroundExpr mTokenPos s =
  left EParse $
  parseExprWithToken parseQName' (parseExpr parseSomeValueLiteral)
    (mTokenPos <|> Just (T.length s - 1)) s

-- Exercise:
--   runIdentity $ runParserT (unParsecT $ parseRequest parseSomeValueLiteral (parseQName' False)) "" "1"
parseExprWithToken ::
  forall (f :: * -> *) (n :: *)
  .  (Bool -> Parser n)
  -> (Parser n -> Parser (Either Text (f n)))
  -> Maybe Int
  -> Text
  -> Fallible (f n)
parseExprWithToken nameParser parser = go False
  where
    go allowToken mTokPos s =
      case (,)
           (runIdentity $ runParserT
            (do
                x <- unParsecT $
                     parser (nameParser allowToken)
                Text.Megaparsec.eof
                pure x)
            "" s)
           mTokPos
      of
        -- If parse succeeds, then propagate immediately.
        (Right (Right x),       _) -> Right x
        -- If we don't parse, and we can't extend, then fail immediately.
        (Left         e,  Nothing) -> fallS $ errorBundlePretty e
        (Right (Left  e), Nothing) -> fallShow e
        -- If we don't parse, and we can extend, then retry with extension allowed.
        (_,           Just tokpos) ->
          go True Nothing (T.take tokpos s <> holeToken <> T.drop tokpos s)
