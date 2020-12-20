{-# LANGUAGE TemplateHaskell #-}
module Lift.Hackage
  ( -- * Namespace
    Lift.Hackage.pipeSpace
  )
where

import Data.Default
import Data.Map qualified                         as Map
import Data.Map                                     (Map)
import Data.Set.Monad qualified                   as Set
import Data.Set.Monad                               (Set)
import Data.Time
import Data.Text
import Data.Vector                                  (Vector)
import Data.Vector qualified                      as Vec
import Generics.SOP qualified                     as SOP
import GHC.Generics

import Control.Concurrent.CachedIO
import Network.HTTP.Req
import System.Exit
import System.Process
import System.IO.Unsafe qualified                 as Unsafe

import Distribution.Hackage.DB qualified          as Hackage
import Distribution.Types.PackageName qualified   as Cabal
import Distribution.PackageDescription qualified  as Cabal
import Distribution.PackageDescription.Parsec qualified as Cabal
import Distribution.Types.GenericPackageDescription qualified as Cabal
---------------- Due to TH picking up TyCons:
import GHC.Maybe qualified
import GHC.Types qualified
import Data.Either qualified
import Distribution.Types.Benchmark qualified
import Distribution.Types.CondTree qualified
import Distribution.Types.ConfVar qualified
import Distribution.Types.Dependency qualified
import Distribution.Types.Executable qualified
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Flag qualified
import Distribution.Types.ForeignLib qualified
import Distribution.Types.Library qualified
import Distribution.Types.TestSuite qualified
import Distribution.Types.PackageDescription qualified
import Distribution.Types.PackageId qualified
import Distribution.Types.UnqualComponentName qualified
import Distribution.Types.Version qualified
import Distribution.Types.VersionRange qualified
import Distribution.Types.VersionRange.Internal qualified
import Distribution.SPDX.License qualified
import Distribution.License qualified
import Distribution.Utils.ShortText qualified
import Distribution.Compiler qualified
import Distribution.Types.SourceRepo qualified
import Distribution.Types.BuildType qualified
import Distribution.Types.SetupBuildInfo qualified
import Distribution.Types.LibraryName qualified
import Distribution.ModuleName qualified
import Distribution.Types.ModuleReexport qualified
import Distribution.Types.LibraryVisibility qualified
import Distribution.Types.BuildInfo qualified
import Distribution.Types.ExecutableScope qualified
import Distribution.Types.LegacyExeDependency qualified
import Distribution.Types.ExeDependency qualified
import Distribution.Types.PkgconfigDependency qualified
import Language.Haskell.Extension qualified
import Distribution.Types.Mixin qualified
---------------- Local
import Basis

import Generics.SOP.Mapping

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Ground
import Dom.Ground.Hask
import Dom.Name
import Dom.Pipe.Pipe
import Dom.Scope
import Dom.Scope.ADTPipe
import Dom.Scope.SomePipe
import Dom.Space.SomePipe

import Ground.Table()

import Lift.Orphanage()



-- instance Serialise Distribution.Types.Version.Version
-- instance Serialise Hackage.VersionData
-- instance Serialise GenericPackageDescription
-- instance Serialise Distribution.Types.Library.Library
-- instance Serialise Distribution.ModuleName.ModuleName
-- instance Serialise Distribution.Types.Dependency.Dependency
-- instance Serialise Distribution.Types.VersionRange.Internal.VersionRange
-- instance Serialise Distribution.Types.LibraryName.LibraryName
-- instance Serialise Distribution.Types.UnqualComponentName.UnqualComponentName
-- not exported: instance Serialise Distribution.ModuleName.ShortTextLst

pipeSpace :: QName Scope -> SomePipeSpace Dynamic
pipeSpace graft = emptySomePipeSpace "Hackage"
  & spsAttachScopes graft
      [ pipeScope "Hackage"
        [ somePipe0 "pkgNames" capsTSG         CVSet   hackagePackageNames
        , somePipe0 "pkgs"     capsTS          CVSet   hackagePackages
        , somePipe1 "cabal"    capsTS  CVPoint CVPoint getHackagePackageCabalDesc
        ] ]
  & spsAttachScopes (graft <> "Cabal")
      [ $(dataProjPipeScope (Proxy @Cabal.GenericPackageDescription))
      , $(dataProjPipeScope (Proxy @Cabal.PackageDescription))
      , $(dataProjPipeScope (Proxy @Cabal.SourceRepo))
      , $(dataProjPipeScope (Proxy @Cabal.Library))
      , $(dataProjPipeScope (Proxy @Cabal.Executable))
      , $(dataProjPipeScope (Proxy @Cabal.BuildInfo))
      ]


hackagePackageNames :: IO (Fallible (Vector (Name Package)))
hackagePackageNames = hackageDB <&> fmap (Vec.map (Name . pack . Cabal.unPackageName) . Vec.fromList . Map.keys)

hackagePackages :: IO (Fallible (Vector Hackage.PackageData))
hackagePackages = hackageDB <&> fmap (Vec.fromList . Map.elems)

-- * XXX:  danger lurked in shadows of lazy IO..
hackageDB :: IO (Fallible (Map Cabal.PackageName Hackage.PackageData))
-- hackagePackageNames :: IO (Fallible (Set (Name Package)))
hackageDB =
  Unsafe.unsafePerformIO . Unsafe.unsafeInterleaveIO $
  setupHackageCache 3600
{-# NOINLINE hackageDB #-}


setupHackageCache :: NominalDiffTime -> IO (IO (Fallible (Map Cabal.PackageName Hackage.PackageData)))
setupHackageCache cacheTmo = cachedIO cacheTmo $ do
  code <- system "cabal new-update"
  case code of
    ExitSuccess -> do
      tarball <- Hackage.hackageTarball
      Right <$> Hackage.readTarball Nothing tarball
    ExitFailure x ->
      fallM . pack $ "'cabal update' exit status: " <> show x

getHackagePackageCabalDesc :: Name Package -> IO (Fallible Cabal.GenericPackageDescription)
getHackagePackageCabalDesc pkg@(Name pn) = do
  r <- runReq defaultHttpConfig $
       req GET (https "hackage.haskell.org" /~ ("package" :: String) /~ pn /~ (pn <> ".cabal")) NoReqBody bsResponse mempty
  case responseStatusCode r of
    200  -> do
      let (_, pkg) = Cabal.runParseResult $ Cabal.parseGenericPackageDescription $ responseBody r
      case pkg of
        Left (_, es) -> fallM . pack $ "Errors while parsing "<>unpack pn<>".cabal: "<>(Prelude.concat $ show <$> es)
        Right x -> pure $ Right x
    resp -> fallM . pack $ "Hackage response code: "<>show resp

 -- data Library = Library
 --  libName           :: Maybe UnqualComponentName
 -- , exposedModules    :: [ModuleName]
 -- , reexportedModules :: [ModuleReexport]
 -- , signatures        :: [ModuleName]   -- ^ What sigs need implementations?
 -- , libExposed        :: Bool           -- ^ Is the lib to be exposed by default?
 -- , libBuildInfo      :: BuildInfo

 -- Executable
 -- * exeName :: UnqualComponentName
 -- * modulePath :: FilePath
 -- * exeScope :: ExecutableScope
 -- * buildInfo :: BuildInfo

 -- BuildInfo
 --  * buildable :: Bool
 --  * buildTools :: [LegacyExeDependency]
 --  * buildToolDepends :: [ExeDependency]
 --  * cppOptions :: [String]
 --  * asmOptions :: [String]
 --  * cmmOptions :: [String]
 --  * ccOptions :: [String]
 --  * cxxOptions :: [String]
 --  * ldOptions :: [String]
 --  * pkgconfigDepends :: [PkgconfigDependency]
 --  * frameworks :: [String]
 --  * extraFrameworkDirs :: [String]
 --  * asmSources :: [FilePath]
 --  * cmmSources :: [FilePath]
 --  * cSources :: [FilePath]
 --  * cxxSources :: [FilePath]
 --  * jsSources :: [FilePath]
 -- * hsSourceDirs :: [FilePath]
 -- * otherModules :: [ModuleName]
 -- * virtualModules :: [ModuleName]
 -- * autogenModules :: [ModuleName]
 -- * defaultLanguage :: Maybe Language
 -- * otherLanguages :: [Language]
 -- * defaultExtensions :: [Extension]
 -- * otherExtensions :: [Extension]
 -- * oldExtensions :: [Extension]
 -- * extraLibs :: [String]
 -- * extraGHCiLibs :: [String]
 -- * extraBundledLibs :: [String]
 -- * extraLibFlavours :: [String]
 -- * extraLibDirs :: [String]
 -- * includeDirs :: [FilePath]
 -- * includes :: [FilePath]
 -- * installIncludes :: [FilePath]
 -- * options :: [(CompilerFlavor, [String])]
 -- * profOptions :: [(CompilerFlavor, [String])]
 -- * sharedOptions :: [(CompilerFlavor, [String])]
 -- * staticOptions :: [(CompilerFlavor, [String])]
 -- * customFieldsBI :: [(String, String)]
 -- * targetBuildDepends :: [Dependency]
 -- * mixins :: [Mixin]

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

 -- GenericPackageDescription
 -- * packageDescription :: PackageDescription
 -- * genPackageFlags    :: [Flag]
 -- * condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library)
 -- * condSubLibraries   :: [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
 -- * condForeignLibs    :: [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)]
 -- * condExecutables    :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
 -- * condTestSuites     :: [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
 -- * condBenchmarks     :: [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
