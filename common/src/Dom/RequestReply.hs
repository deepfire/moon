module Dom.RequestReply (module Dom.RequestReply) where

import qualified Data.ByteString.Char8            as BSC
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import qualified Data.Set.Monad                   as S
import qualified Data.Text                        as Text
import qualified Generics.SOP                     as SOP
import           GHC.Generics                       (Generic)
import qualified Options.Applicative              as Opt
import           Options.Applicative         hiding (Parser)
--import           Text.Megaparsec                  as M

import Basis
import Data.Parsing

import Dom.Expr
import Dom.Error
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.SomeValue

import Ground.Expr
import Ground.Table


--------------------------------------------------------------------------------
-- | Request/Reply:  asks with expectance of certain type of reply.
--
data Request n
  = Run
    { reqExpr :: Expr n }
  | Let
    { reqName :: QName Pipe
    , reqExpr :: Expr n
    }
  deriving (Generic)
  deriving (Show) via (Quiet (Request n))

newtype Reply
  = ReplyValue SomeValue

type StandardRequest = Request (Located (QName Pipe))

instance Show Reply where show (ReplyValue n) = "ReplyValue " <> show n

--------------------------------------------------------------------------------
-- * Parsing
--
parseGroundRequest :: Maybe Int -> Text -> PFallible StandardRequest
parseGroundRequest mTokenPos s =
  left EParse $
  parseExprWithToken parseQName' (parseRequest parseSomeValueLiteral)
    (mTokenPos <|> Just (Text.length s - 1)) s

parseRequest ::
     Parser SomeValue
  -> Parser n
  -> Parser (Either Text (Request n))
parseRequest lit name =
  try (parseLet lit name <* eof)
  <|>  parseRun lit name

parseRun ::
     Parser SomeValue
  -> Parser n
  -> Parser (Either Text (Request n))
parseRun litP nameP =
  fmap Run <$> parseExpr litP nameP

parseLet ::
     Parser SomeValue
  -> Parser n
  -> Parser (Either Text (Request n))
parseLet litP nameP = do
  name <- parseQName
  _    <- token (string "=")
  expr <- parseExpr litP nameP
  pure $ Let name <$> expr

cliRequest :: Opt.Parser (Request (Located (QName Pipe)))
cliRequest = subparser $ mconcat
  [ cmd "run" $
    Run
      <$> pipedesc
  , cmd "let" $
    Let
      <$> (QName <$> argument auto (metavar "NAME"))
      <*> pipedesc
  ]
  where
    cmd name p = command name $ info (p <**> helper) mempty
    pipedesc = argument (eitherReader
                          (bimap (Text.unpack . showError . unEPipe) id
                           . parseGroundExpr Nothing . pack))
               (metavar "PIPEDESC")

--------------------------------------------------------------------------------
-- | Serialise instances
--
tagRequest, tagReply :: Word
tagRequest   = 31--415926535
tagReply     = 27--182818284

instance (Serialise n, Typeable n) => Serialise (Request n) where
  encode x = case x of
    Run e -> encodeListLen 2 <> encodeWord (tagRequest + 0) <> encode e
    Let n e -> encodeListLen 3 <> encodeWord (tagRequest + 1) <> encode n <> encode e
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 31) -> Run <$> decode
      (3, 32) -> Let <$> decode <*> decode
      _ -> failLenTag len tag

instance Serialise Reply where
  encode x = case x of
    ReplyValue v -> encodeListLen 2 <> encodeWord tagReply <> encode (v :: SomeValue)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, _tagSomeReply) -> ReplyValue <$> (decode :: Decoder s SomeValue)
      _ -> failLenTag len tag

failLenTag :: forall s a. Typeable a => Int -> Word -> Decoder s a
failLenTag len tag = fail $ "invalid "<>show (typeRep @a)<>" encoding: len="<>show len<>" tag="<>show tag
