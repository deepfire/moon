module Dom.Expr (module Dom.Expr) where

import           Codec.Serialise                    (Serialise(..))
import           Control.Monad                      (foldM)
import           Data.Either                        (partitionEithers)
import qualified Data.IntervalMap.FingerTree      as IMap
import           Data.Text                          (Text, pack)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Text.Read                          (Read(..))
import           Quiet

import Data.Parsing
import Data.Orphanage

import Dom.Located
import Dom.SomeValue


--------------------------------------------------------------------------------
-- * Types
--
data Expr p where
  PVal  ::
    { vX  :: SomeValue -- so, this makes expressions non-serialisable,
                       -- which doesn't seem to be a good reason
    } -> Expr p
  PPipe ::
    { pP  :: p
    } -> Expr p
  PApp  ::
    { apF :: Expr p
    , apX :: Expr p
    } -> Expr p
  PComp ::
    { coF :: Expr p
    , coG :: Expr p
    } -> Expr p
  deriving (Foldable, Functor, Generic, Traversable)
  deriving (Show) via (Quiet (Expr p))

instance Typeable p => Read (Expr p) where readPrec = failRead

--------------------------------------------------------------------------------
-- * Instances
--
instance Serialise p => Serialise (Expr p)

--------------------------------------------------------------------------------
-- * Parsing
--
parseExpr
  :: forall e n
  . ( e ~ Text)
  => Parser SomeValue
  -> Parser n
  -> Parser (Either e (Expr n))
parseExpr litParser nameParser =
  comps
 where
   term
     =   try (pure . PVal  <$> parseSomeValue litParser)
     <|> try (parens comps)
     <|>     (pure . PPipe <$> nameParser)
   applys = do
     xss <- some term
     case xss of
       x : xs -> foldM (\l r -> pure $ PApp <$> l <*> r) x xs
       _ -> error "Invariant failed: 'some' failed us."
   comps = do
     xs' <- sepBy1 applys (token (string ".")) -- Text.Megaparsec.Char.string?
     let (errs, xs) = partitionEithers xs'
     pure $ if null errs
       then Right $ foldl1 PComp xs
       else Left (pack . show $ head errs)

--------------------------------------------------------------------------------
-- * Utilities
--
indexLocated :: Foldable f => f (Located a)
             -> IMap.IntervalMap Int a
indexLocated =
  foldMap (\Locn{locSpan, locVal} ->
             IMap.singleton locSpan locVal)

lookupLocated
  :: Int -> IMap.IntervalMap Int a
  -> Maybe a
lookupLocated col imap =
  case IMap.search col imap of
    [] -> Nothing
    (_, x):_ -> Just x
