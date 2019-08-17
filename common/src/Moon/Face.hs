{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
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
  , SomeHaskellReply(..)
  )
where

import qualified Algebra.Graph                    as G
import           Control.Monad.Class.MonadST
import           Data.Foldable                      (asum)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Options.Applicative

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

instance Show (HaskellRequest k a) where
  show (Indexes)          = "Indexes"
  show (PackageRepo x y)  = "PackageRepo "    <> show x <> " " <> show y
  show (RepoPackages  x)  = "RepoPackages "   <> show x
  show (PackageModules x) = "PackageModules " <> show x
  show (ModuleDeps     x) = "ModuleDeps "     <> show x
  show (ModuleDefs     x) = "ModuleDefs "     <> show x
  show (DefLoc         x) = "DefLoc "         <> show x

data SomeHaskellRequest where
  SomeHaskellRequest :: HaskellRequest k a -> SomeHaskellRequest

instance Show SomeHaskellRequest where
  show (SomeHaskellRequest x) = show x

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

data SomeHaskellReply
  = PlyIndexes         (Reply List  Index)
  | PlyRepoURL         (Reply Point URL)
  | PlyRepoPackages    (Reply List  Package)
  | PlyPackageModules  (Reply Tree  Module)
  | PlyModuleDeps      (Reply Tree  Package)
  | PlyModuleDefs      (Reply List  Package)
  | PlyDefLoc          (Reply Point Loc)
  deriving (Generic, Show)
