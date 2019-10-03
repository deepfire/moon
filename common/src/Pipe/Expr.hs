{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}

module Pipe.Expr
  ( Expr(..)
  , parse
  )
where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (foldM)

import qualified Text.Parsec.Prim as Parsec
import Text.Parser.Combinators ((<?>), some)
import Text.Parser.Expression
import Text.Parser.Token (TokenParsing, parens, reserve)
import Text.Parser.Token.Style (emptyOps)

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

parse
  :: forall e
  . (e ~ Text)
  => Text
  -> (Either e (Expr (QName SomePipe)))
parse s = do
  case Parsec.parse parseExpr "" ("("<>s<>")") of
    -- XXX: get rid of the parens
    Left         e  -> Left $ "Pipe expr parser: " <> pack (show e)
    Right (Left  e) -> Left $ pack (show e)
    Right (Right x) -> Right $ x

parseExpr
  :: forall e m
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => m (Either e (Expr (QName SomePipe)))
parseExpr =
  buildExpressionParser table term
  <?> "Expr"
 where
   term :: (Monad m, TokenParsing m) => m (Either e (Expr (QName SomePipe)))
   term = parens applys
          <|> (pure . PPipe <$> parseQName)
          <|> (pure . PVal  <$> parseSomeValue)
          <?> "Expr.term"
   applys :: (Monad m, TokenParsing m) => m (Either e (Expr (QName SomePipe)))
   applys = do
     xss <- some parseExpr
     case xss of
       x : xs -> foldM (\l r -> pure $ PApp <$> l <*> r) x xs
       _ -> error "Invariant failed: 'some' failed us."
     <?> "Expr.applys"
   table :: (Monad m, TokenParsing m) => [[Operator m (Either e (Expr (QName SomePipe)))]]
   table =
     [ -- (.) :: (b -> c) -> (a -> b) -> a -> c infixr 9
       [ binary "." (liftA2 PComp) AssocRight
       ]
     ]
   binary name fun assoc = Infix (fun <$ reservedOp name) assoc
   reservedOp name = reserve emptyOps name

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show p => Show (Expr p) where
  show (PVal    x) =   "Val "<>show x
  show (PPipe   x) =  "Pipe "<>show x
  show (PApp  f x) =  "App ("<>show f<>") ("<>show x<>")"
  show (PComp f g) = "Comp ("<>show f<>") ("<>show g<>")"
