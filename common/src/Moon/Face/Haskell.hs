{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
--{-# LANGUAGE UndecidableInstances       #-}

module Moon.Face.Haskell
  (
  -- Generic-ish
    FileName(..)
  , Loc(..)
  , URL(..)
  -- Atomics
  , IndexName(..)
  , RepoName(..)
  , PackageName(..)
  , ModuleName(..)
  , DefName(..)
  -- Composites
  , Index(..)
  , Repo(..)
  , Package(..)
  , Module(..)
  , DefType(..)
  , Def(..)
  , )
where

import Algebra.Graph
import Data.Map
import Codec.Serialise
import Data.Set.Monad
import Data.String
import Data.Text
import GHC.Generics

import Data.Orphanage ()

{-------------------------------------------------------------------------------
  Generic types for externalisation
-------------------------------------------------------------------------------}
newtype URL        = URL        Text deriving (Serialise, Eq, Generic, IsString, Ord, Show)
newtype FileName   = FileName   Text deriving (Serialise, Eq, Generic, IsString, Ord, Show)
-- | 1:1 translation of RealSrcSpan
data Loc
  =  Loc
     { srcSpanFile   :: !FileName,
       srcSpanSLine  :: {-# UNPACK #-} !Int,
       srcSpanSCol   :: {-# UNPACK #-} !Int,
       srcSpanELine  :: {-# UNPACK #-} !Int,
       srcSpanECol   :: {-# UNPACK #-} !Int
     }
     deriving (Eq, Generic, Ord, Show)
instance Serialise  Loc

{-------------------------------------------------------------------------------
  Atomics
-------------------------------------------------------------------------------}
newtype IndexName   = IndexName   Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)
newtype RepoName    = RepoName    Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)
newtype PackageName = PackageName Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)
newtype ModuleName  = ModuleName  Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)
newtype DefName     = DefName     Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)

{-------------------------------------------------------------------------------
  Composites
-------------------------------------------------------------------------------}
-- | Package index -- something we might consult for a package's URL.
data Index = Index
  { ixName           :: !IndexName
  , ixURL            :: !URL
  , ixPackages       :: Map PackageName URL
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Index

data Repo = Repo
  { repoName         :: !RepoName
  , repoURLs         :: Set URL
  , repoPackages     :: Map PackageName Package
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Repo

-- Not strict, since there's a hope of making it actually lazy.
data Package = Package
  { pkgName          :: !PackageName
  , pkgModules       :: Map ModuleName Module
  , pkgModuleDeps    :: Graph Module
  , pkgDeps          :: Set PackageName
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Package

data Module = Module
  { modName          :: !ModuleName
  , modDefs          :: !(Map DefName Def)
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Module

data Def = Def
  { defType          :: !DefType
  , defName          :: !DefName
  , defLoc           :: !Loc
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Def

data DefType
  = TypeSyn
  | TypeFam
  | DataFam
  | Data
  | Class
  | ClassInst !DefName
  | Fun
  | Var
  | Foreign
  deriving (Eq, Generic, Ord, Show)
instance Serialise DefType

