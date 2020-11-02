module Dom.Command (module Dom.Command) where

import           Data.Text                          (Text)
import Data.Parsing

import Dom.Expr
import Dom.Name
import Dom.Pipe
import Dom.SomeValue


--------------------------------------------------------------------------------
-- * Types
--
data Command n
  = Run
    { cmdExpr :: Expr n
    }
  | Let
    { cmdExpr :: Expr n
    , cmdName :: QName Pipe
    }

--------------------------------------------------------------------------------
-- * Parsing
--
parseCommand
  :: forall e n
  . ( e ~ Text)
  => Parser SomeValue
  -> Parser n
  -> Parser (Either e (Command n))
parseCommand litParser nameParser =
      fmap Run <$> parseExpr litParser nameParser
  <|> (do
          name <- parseQName
          _ <- token (string "=")
          expr <- parseExpr litParser nameParser
          pure $ flip Let name <$> expr)
