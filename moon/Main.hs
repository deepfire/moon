{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}

module Main (main) where

import           Data.Text

import           System.Environment

import           Moon.Face.Haskell
import           Moon.Lift.Haskell

main :: IO ()
main = do
  args <- getArgs
  libDir <- getEnv "NIX_GHC_LIBDIR"
  case args of
    [file] -> putStrLn =<< show <$> fileToModule (GhcLibDir libDir) (FileName $ pack file)
    _ -> fail "Exactly one file argument required"
