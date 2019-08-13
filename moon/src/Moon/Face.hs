{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Moon.Face
  ( -- Generic
    Reply(..)
    -- Specific
  , RequestHaskell(..)
  )
where

import Algebra.Graph      (Graph)
import GHC.Generics       (Generic)
import Data.Binary        (Binary)
import Data.Text          (Text)
-- Locals
import Moon.Face.Haskell

{-------------------------------------------------------------------------------
  Generic reply
-------------------------------------------------------------------------------}
data Reply a
  = Sorry Text
  | List        [a]
  | Tree  (Graph a)
  | Dag   (Graph a)
  | Graph (Graph a)
  deriving (Generic)
instance (Binary a, Binary (Graph a)) => Binary (Reply a)

{-------------------------------------------------------------------------------
  Requests (to be compartmentalised..)
-------------------------------------------------------------------------------}
data RequestHaskell
  = Indexes
  | RepoURL          IndexName RepoName
  | RepoPackages     RepoName URL
  | PackageModules   PackageName
  | ModuleDeps       ModuleName
  | ModuleDefs       ModuleName
  | DefLoc           DefName
  deriving Generic
instance Binary RequestHaskell
