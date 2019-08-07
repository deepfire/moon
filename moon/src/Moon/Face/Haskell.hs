{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Moon.Face.Haskell
  ( PkgName(..)
  , ModName(..)
  , DefName(..)
  , FileName(..)
  , Package(..)
  , Module(..)
  , DefType(..)
  , Def(..)
  , Loc(..)
  )
where

import Algebra.Graph
import Data.Map
import Data.Set
import Data.String
import Data.Text

newtype PkgName  = PkgName Text  deriving (Eq, IsString, Ord, Show)
newtype ModName  = ModName Text  deriving (Eq, IsString, Ord, Show)
newtype DefName  = DefName Text  deriving (Eq, IsString, Ord, Show)
newtype FileName = FileName Text deriving (Eq, IsString, Ord, Show)

data Package = Package
  { pkgName       :: PkgName
  , pkgModules    :: Map ModName Module
  , pkgModuleDeps :: Graph Module
  , pkgDeps       :: Set PkgName
  }

data Module = Module
  { modName    :: ModName
  , modDefs    :: Map DefName Def
  } deriving (Show)

data Def = Def
  { defType :: DefType
  , defName :: DefName
  , defLoc  :: Loc
  } deriving (Show)

data DefType
  = TypeSyn
  | TypeFam
  | DataFam
  | Data
  | Class
  | ClassInst DefName
  | Fun
  | Var
  | Foreign
  deriving (Eq, Show)

-- | 1:1 translation of RealSrcSpan
data Loc
  =  Loc
     { srcSpanFile     :: FileName,
       srcSpanSLine    :: {-# UNPACK #-} Int,
       srcSpanSCol     :: {-# UNPACK #-} Int,
       srcSpanELine    :: {-# UNPACK #-} Int,
       srcSpanECol     :: {-# UNPACK #-} Int
     }
     deriving Show
