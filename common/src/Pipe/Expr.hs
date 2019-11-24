module Pipe.Expr
  ( Expr(..)
  , parse
  , parseLocated
  , indexLocated
  , lookupLocatedQName
  )
where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (foldM)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import qualified Data.IntervalMap.FingerTree            as IMap

import Text.Parsec (ParsecT, SourcePos, sourceColumn)
import Text.Parsec.Expr
import qualified Text.Parsec as Parsec
import Text.Parser.Combinators ((<?>), some)
-- import Text.Parser.Expression
import Text.Parser.Token (parens, reserve)
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

parse :: Text -> Either Text (Expr (QName Pipe))
parse = parse' parseQName

indexLocated
  :: Expr (SourcePos, QName Pipe, SourcePos)
  -> IMap.IntervalMap Int (QName Pipe)
indexLocated =
  fold
  . fmap (\(sourceColumn -> l, x, sourceColumn -> r) ->
            IMap.singleton (IMap.Interval l r) x)

lookupLocatedQName
  :: Int -> IMap.IntervalMap Int (QName Pipe)
  -> Maybe (QName Pipe)
lookupLocatedQName col imap =
  case IMap.search col imap of
    [] -> Nothing
    (_, x):_ -> Just x

parseLocated :: Text -> Either Text (Expr (SourcePos, QName Pipe, SourcePos))
parseLocated = parse' parseQNameLocated
 where
   parseQNameLocated =
     (,,)
     <$> Parsec.getPosition
     <*> parseQName
     <*> Parsec.getPosition

parse'
  :: forall e n
  . (e ~ Text)
  => ParsecT Text () Identity n
  -> Text
  -> Either e (Expr n)
parse' nameParser s = do
  case Parsec.parse (parseExpr nameParser) "" ("("<>s<>")") of
    -- XXX: get rid of the parens
    Left         e  -> Left $ "Pipe expr parser: " <> pack (show e)
    Right (Left  e) -> Left $ pack (show e)
    Right (Right x) -> Right $ x

parseExpr
  :: forall e u m n
  . ( e ~ Text
    , Monad m)
  => ParsecT Text u m n
  -> ParsecT Text u m (Either e (Expr n))
parseExpr nameParser =
  buildExpressionParser table term
  <?> "Expr"
 where
   term :: ParsecT Text u m (Either e (Expr n))
   term = parens applys
          <|> (Right . PPipe <$> nameParser)
          <|> (pure . PVal  <$> parseSomeValue)
          <?> "Expr.term"
   applys :: ParsecT Text u m (Either e (Expr n))
   applys = do
     xss <- some (parseExpr nameParser)
     case xss of
       x : xs -> foldM (\l r -> pure $ PApp <$> l <*> r) x xs
       _ -> error "Invariant failed: 'some' failed us."
     <?> "Expr.applys"
   table :: [[Operator Text u m (Either e (Expr n))]]
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
