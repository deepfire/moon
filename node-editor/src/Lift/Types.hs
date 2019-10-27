{-# LANGUAGE NamedFieldPuns #-}
module Lift.Types where

import Common.Prelude
import Searcher.Data.Class
       (SearcherData (..), SearcherHint (..))

import qualified Type.Reflection as R
import Basis
import Pipe
import Type


data Hint
  = HintPipe (QName Pipe) SomeDesc
  deriving (Eq, Generic, Show)

instance NFData Hint

instance SearcherData Hint where
  text = to $ \case
    HintPipe pn _ -> showQName pn
  typeOf = to $ \case
    HintPipe _ (SomeDesc pd) -> showSig $ pdSig pd

instance SearcherHint Hint where
  prefix = to $ \case
    HintPipe _ _ -> "pipePre"
  documentation = to $ \case
    HintPipe _ _ -> "pipeDoc"
