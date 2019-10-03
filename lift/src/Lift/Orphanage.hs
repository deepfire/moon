{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Lift.Orphanage () where

import qualified Generics.SOP as SOP
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal


instance SOP.Generic         Cabal.GenericPackageDescription
instance SOP.HasDatatypeInfo Cabal.GenericPackageDescription

instance SOP.Generic         Cabal.PackageDescription
instance SOP.HasDatatypeInfo Cabal.PackageDescription

instance SOP.Generic         Cabal.SourceRepo
instance SOP.HasDatatypeInfo Cabal.SourceRepo
instance SOP.Generic         Cabal.Library
instance SOP.HasDatatypeInfo Cabal.Library
instance SOP.Generic         Cabal.Executable
instance SOP.HasDatatypeInfo Cabal.Executable
instance SOP.Generic         Cabal.BuildInfo
instance SOP.HasDatatypeInfo Cabal.BuildInfo

