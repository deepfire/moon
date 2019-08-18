{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE RecordWildCards            #-}

module Moon.Lift.Hackage
  ( setupHackageCache
  , PackageCabalDesc
  , getHackagePackageCabalDesc
  )
where

import           Data.Default
import qualified Data.Map                       as Map
import           Data.Map                         (Map)
import qualified Data.Set                       as Set
import           Data.Set                         (Set)
import           Data.Time
import           Data.Text

import           Control.Concurrent.CachedIO
import           Network.HTTP.Req
import           System.Exit
import           System.Process

import qualified Distribution.Hackage.DB        as Hackage
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.GenericPackageDescription

import Moon.Face
import Moon.Face.Haskell

setupHackageCache :: NominalDiffTime -> IO (IO (Either Text (Set PackageName)))
setupHackageCache cacheTmo = cachedIO cacheTmo $ do
  code <- system "cabal new-update"
  case code of
    ExitSuccess -> do
      tarball <- Hackage.hackageTarball
      Right . Set.fromList . (PackageName . pack . Cabal.unPackageName <$>) . Map.keys
        <$> Hackage.readTarball Nothing tarball
    ExitFailure x -> do
      pure . Left . pack $ "'cabal update' exit status: " <> show x

type PackageCabalDesc = Distribution.Types.GenericPackageDescription.GenericPackageDescription

getHackagePackageCabalDesc :: PackageName -> IO (Either Text PackageCabalDesc)
getHackagePackageCabalDesc pkg@(PackageName pn) = do
  r <- runReq def $
       req GET (https "hackage.haskell.org" /~ ("package" :: String) /~ pn /~ (pn <> ".cabal")) NoReqBody bsResponse mempty
  case responseStatusCode r of
    200  -> do
      let (_, pkg) = Cabal.runParseResult $ Cabal.parseGenericPackageDescription $ responseBody r
      case pkg of
        Left (_, es) -> pure . Left . pack $ "Errors while parsing "<>unpack pn<>".cabal: "<>(Prelude.concat $ show <$> es)
        Right x -> pure $ Right x
    resp -> pure . Left . pack $ "Hackage response code: "<>show resp
