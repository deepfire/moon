module Dom.Name (module Dom.Name) where

import Codec.Serialise (Serialise)
import Control.DeepSeq
import Data.Foldable (toList)
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Sequence
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as Text
import qualified Unsafe.Coerce                    as Unsafe

import Data.Parsing

import Dom.Located


--------------------------------------------------------------------------------
-- * Generic names
--
newtype Name a
  = Name { showName :: Text }
  deriving (Eq, Generic, NFData, Ord, Read, Serialise, Typeable)

newtype QName a
  = QName { unQName :: Seq (Name a) }
  deriving (Eq, Generic,         Ord, Read, Serialise, Typeable)

--------------------------------------------------------------------------------
instance Show (Name  a) where show = Text.unpack . showName
instance Show (QName a) where show = Text.unpack . showQName

instance IsString (Name  a) where fromString = Name . Text.pack
instance IsString (QName a) where fromString = textQName . Text.pack

instance Semigroup (QName a) where
  QName l <> QName r = QName (l <> r)

instance Monoid (QName a) where
  mempty = QName mempty

--------------------------------------------------------------------------------
qname :: Name a -> QName a
qname = QName . Seq.singleton

append :: QName a -> Name a -> QName a
append (QName xs) x = QName $ xs Seq.|> x

prepend :: Name a -> QName a -> QName a
prepend x (QName xs) = QName $ x Seq.<| xs

unconsQName :: QName a -> Maybe (QName a, Name a)
unconsQName (QName xs) = case Seq.viewr xs of
  Seq.EmptyR -> Nothing
  pfx Seq.:> n -> Just (QName pfx, n)

lastQName :: QName a -> Name a
lastQName q@(QName xs) = case Seq.viewr xs of
  Seq.EmptyR -> error $ "lastQName:  invoked on" <> show q
  _ Seq.:> n -> n

textQName :: Text -> QName a
textQName = QName . (Name <$>) . Seq.fromList . Text.split (== '.')

listQName :: [Name a] -> QName a
listQName = QName . Seq.fromList

showQName :: QName a -> Text
showQName (QName s) = Text.intercalate "." $ flip fmap (toList s) $
  \(Name x) -> x

coerceName :: Name a -> Name b
coerceName = Unsafe.unsafeCoerce

coerceQName :: QName a -> QName b
coerceQName = Unsafe.unsafeCoerce


-- * Parsing
--
parseName
  :: forall e m a
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => m (Name a)
parseName = parseName' False

parseName'
  :: forall e m a
  . ( e ~ Text
    , Monad m
    , TokenParsing m)
  => Bool -> m (Name a)
parseName' allowHole =
  Name <$> (if allowHole
            then nameIdentifier <|> pure holeToken
            else nameIdentifier)
  <?> "Name"

nameIdentifier :: (Monad m, TokenParsing m) => m Text
nameIdentifier = ident $ IdentifierStyle
  { _styleName = "Name"
  , _styleStart = alphaNum
  , _styleLetter = alphaNum <|> char '.'
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

-- An internal token that serves as a hint to allow the expression to be extended openly.
holeToken :: Text
holeToken = "!"

parseQName
  :: forall e a
  . (e ~ Text)
  => ParsecT e Text Identity (QName a)
parseQName = locVal <$> parseQName' False

parseQName'
  :: forall e a. (e ~ Text)
  => Bool -> ParsecT Text Text Identity (Located (QName a))
parseQName' allowHole =
  if allowHole
  then doParse tok <|> doParse (QName mempty
                                 <$ string (unpack holeToken))
  else doParse tok
 where
   tok = textQName <$> alnumTokenDotty
   doParse x = token $ do
     pre <- getOffset
     v <- x
     flip Locn v . Interval pre <$> getOffset
   alnumTokenDotty :: (TokenParsing m, Monad m) => m Text
   alnumTokenDotty = fmap pack . try $
     (:) <$> alphaNum <*> many constituent
       where constituent = alphaNum <|> char '.'
