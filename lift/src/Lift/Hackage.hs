module Lift.Hackage
  ( -- * Namespace
    Lift.Hackage.pipeSpace
  )
where

import           Data.Default
import qualified Data.Map                       as Map
import           Data.Map                         (Map)
import qualified Data.Set.Monad                 as Set
import           Data.Set.Monad                   (Set)
import           Data.Time
import           Data.Text
import qualified Generics.SOP as SOP
import           GHC.Generics

import           Control.Concurrent.CachedIO
import           Network.HTTP.Req
import           System.Exit
import           System.Process
import qualified System.IO.Unsafe                 as Unsafe

import qualified Distribution.Hackage.DB        as Hackage
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal

import Basis

import Dom.CTag
import Dom.Error
import Dom.Ground.Hask
import Dom.Name
import Dom.Pipe.IOA
import Dom.Pipe.SomePipe
import Dom.Scope
import Dom.Scope.ADTPipe
import Dom.Scope.SomePipe
import Dom.Space.SomePipe
import Dom.Value

import Ground.Table

import Lift.Orphanage


pipeSpace :: QName Scope -> SomePipeSpace Dynamic
pipeSpace graft = emptySomePipeSpace "Hackage"
  & spsAttachScopes (graft)
      [ pipeScope "Hackage"
        [ pipe0T "packages"        TSet' hackagePackageNames
        , pipe1T "cabal" TPoint' TPoint' getHackagePackageCabalDesc
        ]
      , emptyPipeScope "Cabal"
        <> (dataProjScope $ Proxy @Cabal.GenericPackageDescription)
        <> (dataProjScope $ Proxy @Cabal.PackageDescription)
        <> (dataProjScope $ Proxy @Cabal.SourceRepo)
        <> (dataProjScope $ Proxy @Cabal.Library)
        <> (dataProjScope $ Proxy @Cabal.Executable)
        <> (dataProjScope $ Proxy @Cabal.BuildInfo)
      ]


-- * XXX:  danger lurked in shadows of lazy IO..
hackagePackageNames :: IO (Fallible [Name Package])
-- hackagePackageNames :: IO (Fallible (Set (Name Package)))
hackagePackageNames =
  fmap (fmap toList) . Unsafe.unsafePerformIO . Unsafe.unsafeInterleaveIO $
  setupHackageCache 3600
{-# NOINLINE hackagePackageNames #-}


setupHackageCache :: NominalDiffTime -> IO (IO (Fallible (Set (Name Package))))
setupHackageCache cacheTmo = cachedIO cacheTmo $ do
  code <- system "cabal new-update"
  case code of
    ExitSuccess -> do
      tarball <- Hackage.hackageTarball
      Right . Set.fromList . (Name . pack . Cabal.unPackageName <$>) . Map.keys
        <$> Hackage.readTarball Nothing tarball
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
 -- { libName           :: Maybe UnqualComponentName
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
