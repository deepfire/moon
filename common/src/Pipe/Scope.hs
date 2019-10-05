{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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

module Pipe.Scope
  ( PipeScope
  , pipeScope
  , emptyPipeScope
  , dataProjScope
  , dataProjScopeG
  -- * Re-exports
  , scope
  , emptyScope
  , scopeNames
  , scopeEntries
  , lookupScope
  , updateScope
  , alterScope
  )
where

import qualified Type.Reflection                  as R

import Basis
import qualified Generics.SOP.Some as SOP
import Namespace
import Pipe.Ops
import Pipe.Types


type PipeScope = Scope Point SomePipe

--------------------------------------------------------------------------------
emptyPipeScope :: Name PipeScope -> PipeScope
emptyPipeScope = Namespace.emptyScope

pipeScope :: Name PipeScope -> [SomePipe] -> PipeScope
pipeScope name pipes = scope name $ zip (somePipeName <$> pipes) pipes

dataProjPipes
  :: forall c u
  . (Typeable c, Typeable u, SOP.HasTypeData c u, c u)
  => Proxy u -> [Pipe c]
dataProjPipes u =
  let d :: SOP.Data SOP.Fun c u
      d = SOP.typeData (Proxy @c) u
      fieldPipe
        :: SOP.Data  SOP.Fun c u
        -> SOP.Ctor  SOP.Fun c u
        -> SOP.Field SOP.Fun c u
        -> Pipe c
      fieldPipe _d _c f =
        case SOP.fAccess f of
          SOP.SomeAccessors
            (SOP.Accessors getter _ :: SOP.Accessors u c a) ->
            link' (Name $ SOP.fName f)
            TPoint'
            TPoint' -- XXX: Kind can be non-Point!
            (pure . Right . getter)
  in [ fieldPipe d c f
     | c <- SOP.dCtors  d
     , f <- SOP.cFields c]

-- TODO: make this a generic operation?
dataProjScope
  :: forall u.
  ( SOP.HasDatatypeInfo u, SOP.Generic u, All2 (SOP.And Typeable Top) (SOP.Code u)
  , Typeable u)
  => Proxy u -> Scope Point SomePipe
dataProjScope  p = dataProjScope' p $ T <$> dataProjPipes p

dataProjScopeG :: forall u. GroundData u => Proxy u -> Scope Point SomePipe
dataProjScopeG p = dataProjScope' p $ G <$> dataProjPipes p

dataProjScope'
  :: forall u. Typeable u
  => Proxy u -> [SomePipe] -> PipeScope
dataProjScope' _p ps = pipeScope name ps
  where name  = Name $ pack $ show (R.typeRepTyCon (typeRep @u))
