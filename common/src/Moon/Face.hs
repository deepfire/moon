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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Moon.Face
  ( -- * Metatheory
    Kind(..)
  , Repr
  , Tag(..)
  , Meta(..)
  , meta
  , tagMeta
    -- * Derivatives
  , PipeTy(..)
  , PipeModel
  , Reply(..)
  , SomeReply(..)
    -- * Too Specific
  , HaskellRequest(..), parseHaskellRequest
  , SomeHaskellRequest(..)
  , SomeHaskellReply(..)
  )
where

import qualified Algebra.Graph                    as G
import           Control.Monad.Class.MonadST
import           Data.Dynamic
import           Data.Foldable                      (asum)
import           Data.Typeable                      (Typeable)
import qualified Data.Set                         as S
import qualified Data.Kind                        as Kind
import           Data.Proxy                         (Proxy(..))
import           GHC.Generics                       (Generic)
import           Options.Applicative
import           Type.Reflection

-- Locals
import Moon.Face.Haskell

{-------------------------------------------------------------------------------
  Metatheory
-------------------------------------------------------------------------------}
data Kind
  = Point
  | List
  | Set
  | Tree
  | Dag
  | Graph
  deriving (Eq, Generic, Ord, Show, Typeable)

type family Repr k a :: Kind.Type where
  Repr Point a =         a
  Repr List  a =      [] a
  Repr Set   a =   S.Set a
  Repr Tree  a = G.Graph a
  Repr Dag   a = G.Graph a
  Repr Graph a = G.Graph a

data Tag (k :: Kind)  a where
  TPoint :: Tag Point a
  TList  :: Tag List  a
  TSet   :: Tag Set   a
  TTree  :: Tag Tree  a
  TDag   :: Tag Dag   a
  TGraph :: Tag Graph a
  deriving (Typeable)

data Meta =
  Meta
  { metaKind :: TyCon
  , metaType :: SomeTypeRep
  } deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  Derivatives
-------------------------------------------------------------------------------}

data PipeTy
  = Output  -- | IO a
    { ptOut :: Meta
    }
  | Link    -- | b → IO c
    { ptOut :: Meta
    , ptIn  :: Meta
    }
  deriving (Eq, Ord)

data PipeModel

data Reply (k :: Kind) a where
  RPoint :: Repr Point a -> Reply Point a
  RList  :: Repr List  a -> Reply List  a
  RSet   :: Repr Set   a -> Reply Set   a
  RTree  :: Repr Tree  a -> Reply Tree  a
  RDag   :: Repr Dag   a -> Reply Dag   a
  RGraph :: Repr Graph a -> Reply Graph a
  deriving (Typeable)

data SomeReply a where
  SomeReply :: Reply k a -> SomeReply a

{-------------------------------------------------------------------------------
  Ancillary
-------------------------------------------------------------------------------}
meta
  :: forall k a. (Typeable k, Typeable a)
  => Proxy k -> Proxy a -> Meta
meta _ pa =
  Meta (typeRepTyCon $ typeRep @k)
       (someTypeRep pa)

tagMeta
  :: forall k a. (Typeable k, Typeable a)
  => Tag k a -> Meta
tagMeta _ =
  meta (Proxy @k) (Proxy @a)

{-------------------------------------------------------------------------------
  Requests (to be compartmentalised..)
-------------------------------------------------------------------------------}
data SomeHaskellRequest where
  SomeHaskellRequest :: HaskellRequest k a -> SomeHaskellRequest

instance Show SomeHaskellRequest where
  show (SomeHaskellRequest x) = show x

data HaskellRequest (k :: Kind) a where
  Indexes            ::                             HaskellRequest Set   Index
  Packages           :: IndexName                -> HaskellRequest Set   PackageName
  PackageRepo        :: IndexName -> PackageName -> HaskellRequest Point URL
  RepoPackages       :: URL                      -> HaskellRequest Set   PackageName
  PackageModules     :: PackageName              -> HaskellRequest Tree  ModuleName
  ModuleDeps         :: ModuleName               -> HaskellRequest Tree  Package
  ModuleDefs         :: ModuleName               -> HaskellRequest Set   Package
  DefLoc             :: DefName                  -> HaskellRequest Point Loc

data SomeHaskellReply
  = PlyIndexes         (Reply Set   Index)
  | PlyPackages        (Reply Set   PackageName)
  | PlyRepoURL         (Reply Point URL)
  | PlyRepoPackages    (Reply Set   PackageName)
  | PlyPackageModules  (Reply Tree  ModuleName)
  | PlyModuleDeps      (Reply Tree  Package)
  | PlyModuleDefs      (Reply Set   Package)
  | PlyDefLoc          (Reply Point Loc)
  deriving (Generic, Show)

parseHaskellRequest :: Parser SomeHaskellRequest
parseHaskellRequest = subparser $ mconcat
  [ cmd "Indexes"         $ SomeHaskellRequest <$> pure Indexes
  , cmd "Packages"        $ SomeHaskellRequest <$> (Packages       <$> (IndexName   <$> opt "index"))
  , cmd "PackageRepo"     $ SomeHaskellRequest <$> (PackageRepo    <$> (IndexName   <$> opt "index")
                                                                   <*> (PackageName <$> opt "package-name"))
  , cmd "RepoPackages"    $ SomeHaskellRequest <$> (RepoPackages   <$> (URL         <$> opt "repo-url"))
  , cmd "PackageModules"  $ SomeHaskellRequest <$> (PackageModules <$> (PackageName <$> opt "package"))
  , cmd "ModuleDeps"      $ SomeHaskellRequest <$> (ModuleDeps     <$> (ModuleName  <$> opt "module"))
  , cmd "ModuleDefs"      $ SomeHaskellRequest <$> (ModuleDefs     <$> (ModuleName  <$> opt "module"))
  , cmd "DefLoc"          $ SomeHaskellRequest <$> (DefLoc         <$> (DefName     <$> opt "def"))
  ]
  where
    cmd name p = command name $ info (p <**> helper) mempty
    opt name   = option auto (long name)

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show a => Show (Tag k a) where
  show TPoint = "TPoint"
  show TList  = "TList"
  show TSet   = "TSet"
  show TTree  = "TTree"
  show TDag   = "TDag"
  show TGraph = "TGraph"

instance Show Meta where
  show (Meta tycon sometyperep) = "Meta "<>show tycon<>" "<>show sometyperep

instance Show a => Show (Reply k a) where
  show (RPoint x) = "RPoint " <> show x
  show (RList  x) = "RList "  <> show x
  show (RSet   x) = "RSet "   <> show x
  show (RTree  x) = "RTree "  <> show x
  show (RDag   x) = "RDag "   <> show x
  show (RGraph x) = "RGraph " <> show x

instance Show (HaskellRequest k a) where
  show (Indexes)          = "Indexes"
  show (Packages       x) = "Packages "       <> show x
  show (PackageRepo  x y) = "PackageRepo "    <> show x <> " " <> show y
  show (RepoPackages   x) = "RepoPackages "   <> show x
  show (PackageModules x) = "PackageModules " <> show x
  show (ModuleDeps     x) = "ModuleDeps "     <> show x
  show (ModuleDefs     x) = "ModuleDefs "     <> show x
  show (DefLoc         x) = "DefLoc "         <> show x
