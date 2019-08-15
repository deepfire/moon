{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Moon.Face
  ( -- Generic
    Kind(..)
  , Reply(..)
  , SomeReply(..)
    -- Specific
  , HaskellRequest(..), parseHaskellRequest
  , SomeHaskellRequest(..)
  , HaskellReply(..)
  )
where

import qualified Algebra.Graph                  as G
import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Data.Foldable      (asum)
import           Data.Text          (Text)
import           Data.Typeable      (Typeable)
import           GHC.Generics       (Generic)
import           Options.Applicative

import qualified Network.TypedProtocol.Channel  as Net
-- Locals
import Moon.Face.Haskell

{-------------------------------------------------------------------------------
  Generic reply
-------------------------------------------------------------------------------}
data Kind
  = Point
  | List
  | Tree
  | Dag
  | Graph
  deriving (Eq, Generic, Ord, Show, Typeable)

data Reply (k :: Kind) a where
  RPoint ::         a  -> Reply Point a
  RList  ::        [a] -> Reply List  a
  RTree  :: G.Graph a  -> Reply Tree  a
  RDag   :: G.Graph a  -> Reply Dag   a
  RGraph :: G.Graph a  -> Reply Graph a
  deriving (Typeable)

instance Show a => Show (Reply k a) where
  show (RPoint x) = "Point " <> show x
  show (RList  x) = "List "  <> show x
  show (RTree  x) = "Tree "  <> show x
  show (RDag   x) = "Dag "   <> show x
  show (RGraph x) = "Graph " <> show x

data SomeReply a where
  SomeReply :: Reply k a -> SomeReply a

instance Serialise a => Serialise (SomeReply a) where
  encode (SomeReply x) = case x of
    RPoint x -> encodeListLen 2 <> encodeWord 0 <> encode x
    RList  x -> encodeListLen 2 <> encodeWord 1 <> encode x
    RTree  x -> encodeListLen 2 <> encodeWord 2 <> encode x
    RDag   x -> encodeListLen 2 <> encodeWord 3 <> encode x
    RGraph x -> encodeListLen 2 <> encodeWord 4 <> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 0) -> SomeReply . RPoint <$> decode
      (2, 1) -> SomeReply . RList  <$> decode
      (2, 2) -> SomeReply . RTree  <$> decode
      (2, 3) -> SomeReply . RDag   <$> decode
      (2, 4) -> SomeReply . RGraph <$> decode
      _      -> fail $ "invalid SomeReply encoding: len="<>show len<>" tag="<>show tag

{-------------------------------------------------------------------------------
  Requests (to be compartmentalised..)
-------------------------------------------------------------------------------}
data HaskellRequest (k :: Kind) a where
  Indexes            ::                             HaskellRequest List  Index
  PackageRepo        :: IndexName -> PackageName -> HaskellRequest Point URL
  RepoPackages       :: URL                      -> HaskellRequest List  Package
  PackageModules     :: PackageName              -> HaskellRequest Tree  Module
  ModuleDeps         :: ModuleName               -> HaskellRequest Tree  Package
  ModuleDefs         :: ModuleName               -> HaskellRequest List  Package
  DefLoc             :: DefName                  -> HaskellRequest Point Loc    

data SomeHaskellRequest where
  SomeHaskellRequest :: HaskellRequest k a -> SomeHaskellRequest

cmd name p = command name $ info (p <**> helper) mempty
opt name   = option auto (long name)

parseHaskellRequest :: Parser SomeHaskellRequest
parseHaskellRequest = subparser $ mconcat
  [ cmd "Indexes"         $ SomeHaskellRequest <$> pure Indexes
  , cmd "PackageRepo"     $ SomeHaskellRequest <$> (PackageRepo    <$> (IndexName   <$> opt "index")
                                                                   <*> (PackageName <$> opt "package-name"))
  , cmd "RepoPackages"    $ SomeHaskellRequest <$> (RepoPackages   <$> (URL         <$> opt "repo-url"))
  , cmd "PackageModules"  $ SomeHaskellRequest <$> (PackageModules <$> (PackageName <$> opt "package"))
  , cmd "ModuleDeps"      $ SomeHaskellRequest <$> (ModuleDeps     <$> (ModuleName  <$> opt "module"))
  , cmd "ModuleDefs"      $ SomeHaskellRequest <$> (ModuleDefs     <$> (ModuleName  <$> opt "module"))
  , cmd "DefLoc"          $ SomeHaskellRequest <$> (DefLoc         <$> (DefName     <$> opt "def"))
  ]

instance Serialise SomeHaskellRequest where
  encode (SomeHaskellRequest x) = case x of
    Indexes          -> encodeListLen 1 <> encodeWord 0
    PackageRepo  x y -> encodeListLen 3 <> encodeWord 1 <> encode x <> encode y
    RepoPackages   x -> encodeListLen 2 <> encodeWord 2 <> encode x
    PackageModules x -> encodeListLen 2 <> encodeWord 3 <> encode x
    ModuleDeps     x -> encodeListLen 2 <> encodeWord 4 <> encode x
    ModuleDefs     x -> encodeListLen 2 <> encodeWord 5 <> encode x
    DefLoc         x -> encodeListLen 2 <> encodeWord 6 <> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (1, 0) -> pure $ SomeHaskellRequest Indexes
      (3, 1) -> SomeHaskellRequest <$> (PackageRepo    <$> decode <*> decode)
      (2, 2) -> SomeHaskellRequest <$> (RepoPackages   <$> decode)
      (2, 3) -> SomeHaskellRequest <$> (PackageModules <$> decode)
      (2, 4) -> SomeHaskellRequest <$> (ModuleDeps     <$> decode)
      (2, 5) -> SomeHaskellRequest <$> (ModuleDefs     <$> decode)
      (2, 6) -> SomeHaskellRequest <$> (DefLoc         <$> decode)
      _      -> fail $ "invalid SomeHaskellRequest encoding: len="<>show len<>" tag="<>show tag

data HaskellReply
  = PlyIndexes         (Reply List  Index)
  | PlyRepoURL         (Reply Point URL)
  | PlyRepoPackages    (Reply List  Package)
  | PlyPackageModules  (Reply Tree  Module)
  | PlyModuleDeps      (Reply Tree  Package)
  | PlyModuleDefs      (Reply List  Package)
  | PlyDefLoc          (Reply Point Loc)
  deriving (Generic, Show)

instance Serialise HaskellReply where
  encode = \case
    PlyIndexes        x -> encodeListLen 2 <> encodeWord 0 <> encode (SomeReply x)
    PlyRepoURL        x -> encodeListLen 2 <> encodeWord 1 <> encode (SomeReply x)
    PlyRepoPackages   x -> encodeListLen 2 <> encodeWord 2 <> encode (SomeReply x)
    PlyPackageModules x -> encodeListLen 2 <> encodeWord 3 <> encode (SomeReply x)
    PlyModuleDeps     x -> encodeListLen 2 <> encodeWord 4 <> encode (SomeReply x)
    PlyModuleDefs     x -> encodeListLen 2 <> encodeWord 5 <> encode (SomeReply x)
    PlyDefLoc         x -> encodeListLen 2 <> encodeWord 6 <> encode (SomeReply x)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 0) -> (decode :: Decoder s (SomeReply Index))   >>= \case SomeReply x@(RList _)  -> pure (PlyIndexes        x)
                                                                    _                      -> fail "invalid PlyIndexes"
      (2, 1) -> (decode :: Decoder s (SomeReply URL))     >>= \case SomeReply x@(RPoint _) -> pure (PlyRepoURL        x)
                                                                    _                      -> fail "invalid PlyRepoURL"
      (2, 2) -> (decode :: Decoder s (SomeReply Package)) >>= \case SomeReply x@(RList _)  -> pure (PlyRepoPackages   x)
                                                                    _                      -> fail "invalid PlyRepoPackages"
      (2, 3) -> (decode :: Decoder s (SomeReply Module))  >>= \case SomeReply x@(RTree _)  -> pure (PlyPackageModules x)
                                                                    _                      -> fail "invalid PlyPackageModules"
      (2, 4) -> (decode :: Decoder s (SomeReply Package)) >>= \case SomeReply x@(RTree _)  -> pure (PlyModuleDeps     x)
                                                                    _                      -> fail "invalid PlyModuleDeps"
      (2, 5) -> (decode :: Decoder s (SomeReply Package)) >>= \case SomeReply x@(RList _)  -> pure (PlyModuleDefs     x)
                                                                    _                      -> fail "invalid PlyModuleDefs"
      (2, 6) -> (decode :: Decoder s (SomeReply Loc))     >>= \case SomeReply x@(RPoint _) -> pure (PlyDefLoc         x)
                                                                    _                      -> fail "invalid PlyDefLoc"
      _      -> fail $ "invalid HaskellReply encoding: len="<>show len<>" tag="<>show tag

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Generic (G.Graph a)
instance Serialise a => Serialise (G.Graph a) where
