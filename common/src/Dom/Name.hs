module Dom.Name (module Dom.Name) where

import Codec.Serialise (Serialise)
import Control.DeepSeq
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.IntervalMap.FingerTree (Interval(..))
import Data.Sequence
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics

import Data.Sequence qualified                    as Seq
import Data.Text qualified                        as Text
import Unsafe.Coerce qualified                    as Unsafe

import Data.Parsing

import Dom.Located

import Basis (trace)


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

unconsQName :: QName a -> Maybe (Name a, QName a)
unconsQName (QName xs) = case Seq.viewl xs of
  Seq.EmptyL -> Nothing
  n Seq.:< pfx -> Just (n, QName pfx)

unsnocQName :: QName a -> Maybe (QName a, Name a)
unsnocQName (QName xs) = case Seq.viewr xs of
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

nameStart, nameConstituent, nameConstituent' :: Char -> Bool
nameStart        c = isAlphaNum c||c=='*'||c=='+'||c=='_'
nameConstituent  c = isAlphaNum c||c=='*'||c=='+'||c=='-'||c=='_'
nameConstituent' c = isAlphaNum c||c=='.'||c=='*'||c=='+'||c=='-'||c=='_'
{-# INLINE nameStart #-}
{-# INLINE nameConstituent #-}
{-# INLINE nameConstituent' #-}

nameIdentifier :: (Monad m, TokenParsing m) => m Text
nameIdentifier = ident $ IdentifierStyle
  { _styleName      = "Name"
  , _styleStart     = satisfy nameStart
  , _styleLetter    = satisfy nameConstituent
  , _styleReserved  = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

-- An internal token that serves as a hint to allow the expression to be extended openly.
holeToken :: Text
holeToken = "!"

parseQName
  :: Parser (QName a)
parseQName = locVal <$> parseQName' False

parseQName'
  :: Bool -> Parser (Located (QName a))
parseQName' allowHole =
  if allowHole
  then try (doParse tok)
       <|> doParse (QName mempty <$ string (unpack holeToken))
  else doParse tok
 where
   tok = textQName <$> alnumTokenDotty
   doParse x = token $ do
     pre <- getOffset
     v <- x
     flip Locn v . Interval pre <$> getOffset
   alnumTokenDotty :: (TokenParsing m, Monad m) => m Text
   alnumTokenDotty = fmap pack . try $
     (:)
     <$> satisfy nameStart
     <*> many (satisfy nameConstituent')
