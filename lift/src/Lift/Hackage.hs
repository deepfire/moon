{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Lift.Hackage
  ( setupHackageCache
  , PackageCabalDesc
  , getHackagePackageCabalDesc
    -- * Namespace
  , Lift.Hackage.spacePipe
  )
where

import           Data.Default
import qualified Data.Map                       as Map
import           Data.Map                         (Map)
import qualified Data.Set.Monad                 as Set
import           Data.Set.Monad                   (Set)
import           Data.Time
import           Data.Text
import           GHC.Generics

import           Control.Concurrent.CachedIO
import           Network.HTTP.Req
import           System.Exit
import           System.Process

import qualified Distribution.Hackage.DB        as Hackage
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal

import Basis
import Ground
import Ground.Hask
import Namespace
import Pipe
import "common" Type

spacePipe :: QName (Scope Point SomePipe) -> Space Point SomePipe
spacePipe graft = mempty
  & attachScopes (graft)
      [ pipeScope "Hackage"
        [ gen "indices" TSet . pure . pure . Set.fromList . (:[]) $
          Index  "hackage" "https://hackage.haskell.org/" mempty
        ]
      ]

setupHackageCache :: NominalDiffTime -> IO (IO (Either Text (Set (Name Package))))
setupHackageCache cacheTmo = cachedIO cacheTmo $ do
  code <- system "cabal new-update"
  case code of
    ExitSuccess -> do
      tarball <- Hackage.hackageTarball
      Right . Set.fromList . (Name . pack . Cabal.unPackageName <$>) . Map.keys
        <$> Hackage.readTarball Nothing tarball
    ExitFailure x -> do
      pure . Left . pack $ "'cabal update' exit status: " <> show x

type PackageCabalDesc = Cabal.GenericPackageDescription

getHackagePackageCabalDesc :: (Name Package) -> IO (Either Text PackageCabalDesc)
getHackagePackageCabalDesc pkg@(Name pn) = do
  r <- runReq def $
       req GET (https "hackage.haskell.org" /~ ("package" :: String) /~ pn /~ (pn <> ".cabal")) NoReqBody bsResponse mempty
  case responseStatusCode r of
    200  -> do
      let (_, pkg) = Cabal.runParseResult $ Cabal.parseGenericPackageDescription $ responseBody r
      case pkg of
        Left (_, es) -> pure . Left . pack $ "Errors while parsing "<>unpack pn<>".cabal: "<>(Prelude.concat $ show <$> es)
        Right x -> pure $ Right x
    resp -> pure . Left . pack $ "Hackage response code: "<>show resp

 -- GenericPackageDescription
 -- * packageDescription :: PackageDescription
 -- * genPackageFlags    :: [Flag]
 -- * condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library)
 -- * condSubLibraries   :: [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
 -- * condForeignLibs    :: [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)]
 -- * condExecutables    :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
 -- * condTestSuites     :: [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
 -- * condBenchmarks     :: [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]

 -- PackageDescription
 -- * specVersionRaw :: Either Version VersionRange
 -- * package        :: PackageIdentifier
 -- * licenseRaw     :: Either License License
 -- * licenseFiles   :: [FilePath]
 -- * copyright      :: String
 -- * maintainer     :: String
 -- * author         :: String
 -- * stability      :: String
 -- * testedWith     :: [(CompilerFlavor, VersionRange)]
 -- * homepage       :: String
 -- * pkgUrl         :: String
 -- * bugReports     :: String
 -- * sourceRepos    :: [SourceRepo]
 -- * synopsis       :: String --  A one-line summary of this package
 -- * description    :: String --  A more verbose description of this package
 -- * category       :: String
 -- * customFieldsPD :: [(String, String)] --  Custom fields starting with x-, stored in a simple assoc-list.
 -- * buildTypeRaw   :: Maybe BuildType
 -- * setupBuildInfo :: Maybe SetupBuildInfo
 -- * library        :: Maybe Library
 -- * subLibraries   :: [Library]
 -- * executables    :: [Executable]
 -- * foreignLibs    :: [ForeignLib]
 -- * testSuites     :: [TestSuite]
 -- * benchmarks     :: [Benchmark]
 -- * dataFiles      :: [FilePath]
 -- * dataDir        :: FilePath
 -- * extraSrcFiles  :: [FilePath]
 -- * extraTmpFiles  :: [FilePath]
 -- * extraDocFiles  :: [FilePath]
