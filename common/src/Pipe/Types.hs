{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE ViewPatterns               #-}

module Pipe.Types
  ( Pipe(..)
  , showPipe
  , showPipeP
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
-- | Pipe: a concrete, runnable 'Def'-inition.
data Pipe = forall (k :: Con) (a :: *)
  . (Ground a, Typeable k)
  => Pipe
  { pipeName :: Name Pipe
  , pipeSig  :: Sig
  , _pStruct :: Struct
  , _pTo     :: Tag k a
  , _pDyn    :: Dynamic -- ^ A wrapped 'PipeFun'.
  }

showPipe, showPipeP :: Pipe -> Text
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
pattern PGen  :: Name Pipe -> Struct -> Dynamic ->         Type -> Pipe
pattern PGen  n st dy    so <- Pipe n (Gen     so) st _ dy

pattern PLink :: Name Pipe -> Struct -> Dynamic -> Type -> Type -> Pipe
pattern PLink n st dy si so <- Pipe n (Link si so) st _ dy

pattern PAny  :: Name Pipe -> Struct -> Dynamic ->         Type -> Pipe
pattern PAny  n st dy    so <- Pipe n (sOut -> so) st _ dy

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show Pipe where
  show p = "Pipe "<>show (pipeName p)<>" "<>show (pipeSig p)

instance Show Sig where
  show (Gen    o)  =  "Gen "<>show o
  show (Link i o)  = "Link "<>show i<>" -> "<>show o
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead
