{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}

module Pipe.Expr
  ( Expr(..)
  , parse
  )
where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (foldM)
import qualified Data.Sequence                    as Seq
import Data.Text (split)

import qualified Text.Parsec.Prim as Parsec
import Text.Parser.Char (alphaNum, char)
import Text.Parser.Combinators ((<?>), some)
import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.Parser.Token
       ( IdentifierStyle(..)
       , TokenParsing
       , ident
       , parens
       , reserve)
import Text.Parser.Token.Style (emptyOps)

import Basis
import Pipe.Types
import Type


data Expr where
  PVal  ::
    { vX  :: SomeValue
    } -> Expr
  PPipe ::
    { pP  :: Pipe
    } -> Expr
  PApp ::
    { apF :: Expr
    , apX :: Expr
    } -> Expr
  PComp ::
    { coF :: Expr
    , coG :: Expr
    } -> Expr
  deriving (Show)

parse
  :: (QName Pipe -> Either Text Pipe)
  -> Text
  -> Either Text Expr
parse lookupPipe s =
  case Parsec.parse (parser lookupPipe) "" s of
    Left err -> Left $ "Pipe expr parser: " <> pack (show err)
    Right x -> x

parser
  :: forall e m
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => (QName Pipe -> Either e Pipe)
  -> m (Either e Expr)
parser lookupPipe =
  buildExpressionParser table term
  <?> "expression"
 where
   term :: (Monad m, TokenParsing m) => m (Either e Expr)
   term = parens applys
          <|> ((PPipe <$>) . lookupPipe <$> pipeName)
          <?> "simple expression"
   applys :: (Monad m, TokenParsing m) => m (Either e Expr)
   applys = do
     xss <- some (parser lookupPipe)
     case xss of
       x : xs -> foldM (\l r -> pure $ PApp <$> l <*> r) x xs
       _ -> error "Invariant failed: 'some' failed us."
   table :: (Monad m, TokenParsing m) => [[Operator m (Either e Expr)]]
   table =
     [ -- (.) :: (b -> c) -> (a -> b) -> a -> c infixr 9
       [ binary "." (liftA2 PComp) AssocRight
       ]
     ]
   pipeName :: (Monad m, TokenParsing m) => m (QName a)
   pipeName = QName . (Name <$>) . Seq.fromList . split (== '.') <$> identifier

   identifier :: (Monad m, TokenParsing m) => m Text
   identifier = ident $ IdentifierStyle
     { _styleName = "pipe name"
     , _styleStart = alphaNum
     , _styleLetter = alphaNum <|> char '.'
     , _styleReserved = mempty
     , _styleHighlight = Identifier
     , _styleReservedHighlight = ReservedIdentifier
     }
   binary name fun assoc = Infix (fun <$ reservedOp name) assoc
   reservedOp name = reserve emptyOps name
