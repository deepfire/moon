{-# LANGUAGE OverloadedStrings #-}
module Pipe.Expr
  ( Expr(..)
  , parse
  , parseLocated
  , indexLocated
  , lookupLocatedQName
  , parseExpr
  )
where

import Control.Applicative (some)
import Control.Monad (foldM)
import Data.Either (partitionEithers)
import qualified Data.IntervalMap.FingerTree            as IMap

import Text.Megaparsec (runParserT, eof)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Parsers (unParsecT, sepBy1)
import Text.Parser.Token

import Data.Parsing
-- import Debug.TraceErr
import Basis
import Ground
import Pipe.Types


data Expr p where
  PVal  ::
    { vX  :: SomeValue
    } -> Expr p
  PPipe ::
    { pP  :: p
    } -> Expr p
  PApp ::
    { apF :: Expr p
    , apX :: Expr p
    } -> Expr p
  PComp ::
    { coF :: Expr p
    , coG :: Expr p
    } -> Expr p
  deriving (Foldable, Functor, Traversable)


parse :: Text -> Either Text (Expr (QName Pipe))
parse = fmap (fmap snd3) <$> parse' parseQName'

indexLocated
  :: Expr (Int, QName Pipe, Int)
  -> IMap.IntervalMap Int (QName Pipe)
indexLocated =
  foldMap (\(l, x, r) ->
             IMap.singleton (IMap.Interval l r) x)

lookupLocatedQName
  :: Int -> IMap.IntervalMap Int (QName Pipe)
  -> Maybe (QName Pipe)
lookupLocatedQName col imap =
  case IMap.search col imap of
    [] -> Nothing
    (_, x):_ -> Just x

parseLocated :: Text -> Either Text (Expr (Int, QName Pipe, Int))
parseLocated = parse' parseQName'

parse'
  :: forall e n. (e ~ Text)
  => (Bool -> Parser n)
  -> Text
  -> Either e (Expr n)
parse' nameParser = tryParse True
 where
   tryParse :: Bool -> Text -> Either e (Expr n)
   tryParse mayExtend s =
     case (,)
          (runIdentity $ runParserT
           (do
               x <- unParsecT $ parseExpr (nameParser (not mayExtend))
               eof
               pure x)
            "" s)
          mayExtend
     of
       (Right (Right x), _)     -> Right x
       (Left         e,  False) -> Left $ "Pipe expr parser: " <> pack (show e)
       (Right (Left  e), False) -> Left $ pack (show e)
       (_,               True)  -> tryParse False (s <> magicToken)

parseExpr
  :: forall e n
  . ( e ~ Text)
  => Parser n
  -> Parser (Either e (Expr n))
parseExpr nameParser =
  comps
 where
   term
     =   parens comps
     <|> (pure . PVal  <$> parseSomeValue)
     <|> (pure . PPipe <$> nameParser)
   applys = do
     xss <- some term
     case xss of
       x : xs -> foldM (\l r -> pure $ PApp <$> l <*> r) x xs
       _ -> error "Invariant failed: 'some' failed us."
   comps = do
     xs' <- sepBy1 applys (token (string "."))
     let (errs, xs) = partitionEithers xs'
     pure $ if null errs
       then Right $ foldl1 PComp xs
       else Left (pack . show $ head errs)

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show p => Show (Expr p) where
  show (PVal    x) =   "Val "<>show x
  show (PPipe   x) =  "Pipe "<>show x
  show (PApp  f x) =  "App ("<>show f<>") ("<>show x<>")"
  show (PComp f g) = "Comp ("<>show f<>") ("<>show g<>")"
