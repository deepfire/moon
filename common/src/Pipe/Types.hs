{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE ViewPatterns               #-}

module Pipe.Types
  ( SomePipe(..)
  , Pipe(..)
  , showPipe
  , showPipeP
  , somePipeName
  , somePipeSig
  , Sig(..)
  , Struct(..)
  , Result
  , pattern PGen
  , pattern PLink
  , pattern PAny
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Data.Dynamic
import           GHC.Generics                       (Generic)

import Basis
import Type

--------------------------------------------------------------------------------
data SomePipe
  = forall c. c ~ Ground => G (Pipe c)
  | forall c. c ~ Top    => T (Pipe c)

somePipeName :: SomePipe -> Name SomePipe
somePipeName (G Pipe{pipeName=Name n}) = Name n
somePipeName (T Pipe{pipeName=Name n}) = Name n

somePipeSig :: SomePipe -> Sig
somePipeSig (G Pipe{pipeSig}) = pipeSig
somePipeSig (T Pipe{pipeSig}) = pipeSig

-- | Pipe: a concrete, runnable 'Def'-inition.
data Pipe (c :: * -> Constraint) where
  Pipe
    :: forall (k :: Con) (a :: *) (c :: * -> Constraint)
    . (Typeable k, c a)
    =>
    { pipeName :: Name Pipe
    , pipeSig  :: Sig
    , pStruct :: Struct
    , pTo     :: Tag k a
    , pDyn    :: Dynamic -- ^ A wrapped 'PipeFun'.
    } -> Pipe c

showPipe, showPipeP :: Pipe c -> Text
showPipe (PAny name sig _ _) = pack $ show name <>" :: "<>show sig
showPipeP x = "("<>showPipe x<>")"

--------------------------------------------------------------------------------
-- | Sig: a structure-oblivious abstraction of a pipe as its endpoints.
data Sig
  = Gen    -- ^  Pipe endpoint: IO a
    { sOut :: Type
    }
  | Link   -- ^ Pipe transform: b → IO c
    { sIn  :: Type
    , sOut :: Type
    }
  deriving (Eq, Generic, Ord)

--------------------------------------------------------------------------------
-- | Struct: Pipe's definition, as a graph of type transformations.
newtype Struct = Struct (G.Graph Type) deriving (Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------
-- | Result of running a pipe.
type Result a = IO (Either Text a)

--------------------------------------------------------------------------------
pattern PGen  :: Name Pipe -> Struct -> Dynamic ->         Type -> Pipe c
pattern PGen  n st dy    so <- Pipe n (Gen     so) st _ dy

pattern PLink :: Name Pipe -> Struct -> Dynamic -> Type -> Type -> Pipe c
pattern PLink n st dy si so <- Pipe n (Link si so) st _ dy

pattern PAny  :: Name Pipe -> Struct -> Dynamic ->         Type -> Pipe c
pattern PAny  n st dy    so <- Pipe n (sOut -> so) st _ dy

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show SomePipe where
  show (G p) = "GPipe "<>unpack (showPipe p)
  show (T p) = "TPipe "<>unpack (showPipe p)

instance Show (Pipe c) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show Sig where
  show (Gen    o)  =  "Gen "<>show o
  show (Link i o)  = "Link "<>show i<>" -> "<>show o
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead
