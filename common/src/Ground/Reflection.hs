{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
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
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Ground.Reflection
  ( -- *
    Variant(..)
  , Field(..)
  , typeVariants
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Control.Monad.Class.MonadST
import           Data.Dynamic
import           Data.Foldable                      (asum)
import           Data.Kind                          (Type)
import qualified Data.Map                         as Map
import           Data.Map                           (Map)
import           Data.Maybe                         (fromMaybe)
import           Data.String                        (IsString)
import           Data.Text                          (Text, pack, unpack)
import qualified Data.Set                         as S
import qualified Data.Kind                        as Kind
import           Data.Proxy                         (Proxy(..))
import           GHC.Generics                hiding ((:.:))
import qualified GHC.Generics                     as GHC
import           Options.Applicative
import           Type.Reflection
import qualified Unsafe.Coerce                    as Unsafe

import Data.Dict
import qualified Generics.SOP.Some as SOP
import Basis
import Type hiding (Type)
import qualified Type as Type

--------------------------------------------------------------------------------
-- | 'Variant' of an ADT
data Variant
  = Variant
    { vName   :: Name Variant
    , vFields :: [Field]
    } deriving (Eq, GHC.Generic, Ord, Show)
instance Serialise Variant
instance Read Variant where readPrec = failRead

--------------------------------------------------------------------------------
-- | 'Field' of a 'Proj'
data Field
  = Field
    { fName :: Name Field
    , fType :: Type
    } deriving (Eq, Generic, Ord, Show)
instance Serialise Field

instance Read Field where readPrec = failRead

--------------------------------------------------------------------------------
typeVariants :: Type.Type -> Maybe [Variant]
typeVariants (Type.Type _ _ str) = someTypeRepVariants str

someTypeRepVariants :: SomeTypeRep -> Maybe [Variant]
someTypeRepVariants str =
  withGroundType str strVariants
  where strVariants :: Dict Ground -> [Variant]
        strVariants (Dict (p :: Proxy a)) =
          undefined

