{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ground.Hask
  (
  -- Generic-ish
    FileName(..)
  , Loc(..)
  , URL(..)
  -- Composites
  , Index(..)
  , Repo(..)
  , Package(..)
  , Module(..)
  , Def(..)
  , DefType(..)
  , )
where

import Algebra.Graph
import Codec.Serialise
import GHC.Generics

import qualified Generics.SOP.Some as SOP

import Basis
import Type

{-------------------------------------------------------------------------------
  Generic types for externalisation
-------------------------------------------------------------------------------}
newtype URL        = URL        Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)
newtype FileName   = FileName   Text deriving (Serialise, Eq, Generic, IsString, Ord, Read, Show)
-- | 1:1 translation of RealSrcSpan
data Loc
  =  Loc
     { srcSpanFile   :: !FileName,
       srcSpanSLine  :: {-# UNPACK #-} !Int,
       srcSpanSCol   :: {-# UNPACK #-} !Int,
       srcSpanELine  :: {-# UNPACK #-} !Int,
       srcSpanECol   :: {-# UNPACK #-} !Int
     }
     deriving (Eq, Generic, Ord, Read, Show)
instance Serialise  Loc
instance SOP.Generic Loc
instance SOP.HasDatatypeInfo Loc

{-------------------------------------------------------------------------------
  Composites
-------------------------------------------------------------------------------}
-- | Package index -- something we might consult for a package's URL.
data Index = Index
  { ixName           :: !(Name Index)
  , ixURL            :: !URL
  , ixPackages       :: Set (Name Package)
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Index
instance SOP.Generic Index
instance SOP.HasDatatypeInfo Index
instance Read Index where readPrec = failRead

data Repo = Repo
  { repoName         :: !(Name Repo)
  , repoURLs         :: Set URL
  , repoPackages     :: Map (Name Package) Package
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Repo
instance SOP.Generic Repo
instance SOP.HasDatatypeInfo Repo
instance Read Repo where readPrec = failRead

-- Not strict, since there's a hope of making it actually lazy.
data Package = Package
  { pkgName          :: !(Name Package)
  , pkgModules       :: Map (Name Module) Module
  , pkgModuleDeps    :: Graph Module
  , pkgDeps          :: Set (Name Package)
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Package
instance SOP.Generic Package
instance SOP.HasDatatypeInfo Package
instance Read Package where readPrec = failRead
instance Read (Graph Module) where readPrec = failRead

data Module = Module
  { modName          :: !(Name Module)
  , modDefs          :: !(Map (Name Def) Def)
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Module
instance SOP.Generic Module
instance SOP.HasDatatypeInfo Module
instance Read Module where readPrec = failRead

data Def = Def
  { defType          :: !DefType
  , defName          :: !(Name Def)
  , defLoc           :: !Loc
  } deriving (Eq, Generic, Ord, Show)
instance Serialise Def
instance SOP.Generic Def
instance SOP.HasDatatypeInfo Def
instance Read Def where readPrec = failRead

data DefType
  = TypeSyn
  | TypeFam
  | DataFam
  | Data
  | Class
  | ClassInst !(Name Def)
  | Fun
  | Var
  | Foreign
  deriving (Eq, Generic, Ord, Read, Show)
instance Serialise DefType
instance SOP.Generic DefType
instance SOP.HasDatatypeInfo DefType
