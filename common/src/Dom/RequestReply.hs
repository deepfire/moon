module Dom.RequestReply (module Dom.RequestReply) where

import qualified Data.ByteString.Char8            as BSC
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import qualified Data.Set.Monad                   as S
import qualified Data.Text                        as Text
import qualified Generics.SOP                     as SOP
import qualified Options.Applicative              as Opt
import           Options.Applicative         hiding (Parser)
import           Text.Megaparsec                  as M

import Data.Parsing

import Basis

import Dom.Expr
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.SomeValue

import Ground.Expr


--------------------------------------------------------------------------------
-- | Request/Reply:  asks with expectance of certain type of reply.
--
data Request
  = Run (Expr (Located (QName Pipe)))
  | Compile (QName Pipe) (Expr (Located (QName Pipe)))

data Reply
  = ReplyValue SomeValue

instance Show Request where
  show (Run       expr) = "Run "                      <> show expr
  show (Compile n expr) = "Compile " <> show n <> " " <> show expr
instance Show Reply where show (ReplyValue n) = "ReplyValue " <> show n

parseRequest :: Opt.Parser Request
parseRequest = subparser $ mconcat
  [ cmd "run" $
    Run
      <$> pipedesc
  , cmd "compile" $
    Compile
      <$> (QName <$> argument auto (metavar "NAME"))
      <*> pipedesc
  ]
  where
    cmd name p = command name $ info (p <**> helper) mempty
    pipedesc = argument (eitherReader
                          (bimap show id . parseGroundExpr . pack))
               (metavar "PIPEDESC")

--------------------------------------------------------------------------------
-- | Serialise instances
--
tagRequest, tagReply :: Word
tagRequest   = 31--415926535
tagReply     = 27--182818284

instance Serialise Request where
  encode x = case x of
    Run e -> encodeListLen 2 <> encodeWord (tagRequest + 0) <> encode e
    Compile n e -> encodeListLen 3 <> encodeWord (tagRequest + 1) <> encode n <> encode e
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 31) -> Run     <$> decode
      (3, 32) -> Compile <$> decode <*> decode
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
