module Ground.Table
  ( lookupRep
  , lookupName
  , withRepGroundType
  , withNameGroundType
  , groundTypeReps
  , groundTypeNames
  --
  , parseDict
  , parseSomeValue
  )
where

import qualified Data.Set.Monad                   as Set
import Text.Read (Lexeme(..), ReadPrec, lexP)
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token.Highlight
import Text.Parser.Token

import Basis
import qualified Data.Dict as Dict
import Data.Dict (Dict(..), Dicts)
import Type

import Ground.Parser ()
import qualified Ground.Hask as Hask
import Namespace
import Pipe.Types


-- * Ground API
--
lookupRep :: SomeTypeRep -> Maybe (Dict Ground)
lookupRep = Dict.lookupRep groundTypes

lookupName :: Text       -> Maybe (Dict Ground)
lookupName = Dict.lookupName groundTypes

withRepGroundType :: SomeTypeRep -> (Dict Ground -> b) -> Maybe b
withRepGroundType  str f = f <$> lookupRep str

withNameGroundType :: Text -> (Dict Ground -> b) -> Maybe b
withNameGroundType str f = f <$> lookupName str

groundTypeReps :: [SomeTypeRep]
groundTypeReps = Dict.reps groundTypes

groundTypeNames :: [Text]
groundTypeNames = Dict.names groundTypes


-- * Dict Ground
--
instance Parser (Dict Ground) where
  parser = parseDict

parseDict :: (Monad m, TokenParsing m) => m (Dict Ground)
parseDict = do
  i <- identifier
  case Ground.Table.lookupName i of
    Just x -> pure x
    Nothing -> trace (printf "Unknown ground: %s" i)
                     (fail $ "Unknown ground: " <> show i)
 where
   identifier :: (Monad m, TokenParsing m) => m Text
   identifier = ident $ IdentifierStyle
     { _styleName = "Ground tag"
     , _styleStart = letter
     , _styleLetter = alphaNum
     , _styleReserved = mempty
     , _styleHighlight = Identifier
     , _styleReservedHighlight = ReservedIdentifier
     }

instance Read (Dict Ground) where
  readPrec = do
    ty <- lexP
    case ty of
      Ident i -> case Ground.Table.lookupName (pack i) of
        Just x -> pure x
        Nothing -> trace (printf "Unknown ground: %s" i)
                         (fail $ "Unknown ground: " <> i)
      x -> fail $ "Unexpected construct: " <> show x


-- * SomeValue
--
instance Parser SomeValue where
  parser = parseSomeValue

parseSomeValue :: (Monad m, TokenParsing m) => m SomeValue
parseSomeValue =
  braces   (parseSV . reifyTag $ Proxy @Point)
  <|>
  brackets (parseSV . reifyTag $ Proxy @List)
  <?> "Value"
 where
  parseSV :: (Monad m, TokenParsing m) => Tag k -> m SomeValue
  parseSV tag = do
    Dict a :: Dict Ground <- parser
    case tag of
      TPoint -> do
        v :: a <- parser
        pure $ SomeValue $ SomeKindValue $ mkValue' a tag v
      TList -> do
        v :: [a] <- commaSep parser
        pure $ SomeValue $ SomeKindValue $ mkValue' a tag v
      TSet -> do
        v :: [a] <- commaSep parser
        pure $ SomeValue $ SomeKindValue $ mkValue' a tag $ Set.fromList v
      _ -> trace (printf "No parser for structures outside of Point/List/Set.")
                 (fail "")

readValue :: forall (k :: Con). Tag k -> Dict Ground -> ReadPrec SomeValue
readValue tag (Dict (a :: Proxy a)) = do
  case tag of
    TPoint -> do
      v :: a <- readPrec
      pure $ SomeValue $ SomeKindValue $ mkValue' a tag v
    TList -> do
      v :: [a] <- readListPrec
      pure $ SomeValue $ SomeKindValue $ mkValue' a tag v
    TSet -> do
      v :: [a] <- readListPrec
      pure $ SomeValue $ SomeKindValue $ mkValue' a tag $ Set.fromList v
    _ -> trace (printf "No parser for structures outside of Point/List/Set.")
               (fail "")

instance Read SomeValue where
  readPrec = do
    tag :: Some Tag <- readPrec
    dict :: Dict Ground <- readPrec
    case tag of
      Exists tag' -> readValue tag' dict


-- * Tables
--
infixr 2 #
(#) :: (a -> b) -> a -> b
(#) = ($)

groundTypes :: Dicts Ground
groundTypes = Dict.empty
  -- Meta
  -- & Dict.insert "Ground"          # Proxy @(Dict Ground)
  & Dict.insert "Con"             # Proxy @Con
  & Dict.insert "Type"            # Proxy @Type
  & Dict.insert "Sig"             # Proxy @Sig
  & Dict.insert "Struct"          # Proxy @Struct
  & Dict.insert "PipeDesc"        # Proxy @PipeDesc
  & Dict.insert "SomeTypeRep"     # Proxy @SomeTypeRep
  & Dict.insert "SomeTypeRep2"    # Proxy @(SomeTypeRep, SomeTypeRep)
  & Dict.insert "NameType"        # Proxy @(Name Type)
  & Dict.insert "NamePipe"        # Proxy @(Name Pipe)
  & Dict.insert "QNamePipe"       # Proxy @(QName Pipe)
  & Dict.insert "QNameScope"      # Proxy @(QName Scope)
  & Dict.insert "QNameScope"      # Proxy @(PipeSpace PipeDesc)
  -- Atom
  & Dict.insert "Int"             # Proxy @Int
  & Dict.insert "Integer"         # Proxy @Integer
  & Dict.insert "Float"           # Proxy @Float
  & Dict.insert "Double"          # Proxy @Double
  & Dict.insert "String"          # Proxy @String
  & Dict.insert "Text"            # Proxy @Text
  & Dict.insert "Unit"            # Proxy @()
  -- Plain
  & Dict.insert "FileName"        # Proxy @Hask.FileName
  & Dict.insert "Loc"             # Proxy @Hask.Loc
  & Dict.insert "URL"             # Proxy @Hask.URL
  -- Hask
  & Dict.insert "NameHaskIndex"   # Proxy @(Name Hask.Index)
  & Dict.insert "HaskIndex"       # Proxy @Hask.Index
  & Dict.insert "NameHaskRepo"    # Proxy @(Name Hask.Repo)
  & Dict.insert "NameHaskPackage" # Proxy @(Name Hask.Package)
  & Dict.insert "NameHaskModule"  # Proxy @(Name Hask.Module)
  & Dict.insert "NameHaskDef"     # Proxy @(Name Hask.Def)
  & Dict.insert "HaskDef"         # Proxy @Hask.Def
  & Dict.insert "HaskDefType"     # Proxy @Hask.DefType
