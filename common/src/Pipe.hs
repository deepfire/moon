{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Pipe
  (
  -- * Namespacery
    dataProjScope
  , dataProjPipes
  , pipeScope
  -- * Re-exports
  , module Pipe.Expr
  , module Pipe.Ops
  , module Pipe.Types
  )
where

import qualified Data.Map                         as Map
import qualified Type.Reflection                  as R

import Basis
import qualified Generics.SOP.Some as SOP
import Namespace
import Pipe.Expr
import Pipe.Ops
import Pipe.Types

--------------------------------------------------------------------------------
dataProjPipes
  :: forall u
  . (Ground u, SOP.HasTypeData Ground u)
  => Proxy u -> [Pipe]
dataProjPipes u =
  let d :: SOP.Data SOP.Fun Ground u
      d = SOP.typeData (Proxy @Ground) u
      fieldPipe
        :: SOP.Data      SOP.Fun Ground u
        -> SOP.Ctor      SOP.Fun Ground u
        -> SOP.Field SOP.Fun Ground u
        -> Pipe
      fieldPipe _d _c f =
        case SOP.fAccess f of
          SOP.SomeAccessors
            (SOP.Accessors getter _ :: SOP.Accessors u Ground a) ->
            link (Name $ SOP.fName f)
            TPoint
            TPoint -- XXX: Kind can be non-Point!
            (pure . Right . getter)
  in [ fieldPipe d c f
     | c <- SOP.dCtors  d
     , f <- SOP.cFields c]

-- TODO: make this a generic operation?
dataProjScope :: forall u. GroundData u => Proxy u -> Scope Point Pipe
dataProjScope u = pipeScope name pipes
  where name  = Name $ pack $ show (R.typeRepTyCon (typeRep @u))
        pipes = dataProjPipes u

pipeScope :: Name (Scope Point Pipe) -> [Pipe] -> Scope Point Pipe
pipeScope name pipes = Scope
  { sName = name
  , sMap  = Map.fromList $ zip (pipeName <$> pipes) pipes
  }
