{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint where

import Common.Prelude

import NodeEditor.React.Model.Searcher.Hint.Command (Command)
import NodeEditor.React.Model.Searcher.Hint.Node    (Node)
import Searcher.Data.Class                          (SearcherData (text), SearcherHint (documentation, prefix))

import Data.Text

import qualified Type
import qualified Pipe

------------------
-- === Hint === --
------------------

-- === Definition === --

data Hint
    = Command Command
    | Node    Node
    | Pipe    (Type.QName Pipe.Pipe)
    deriving (Eq, Generic, Show)

makePrisms ''Hint

instance NFData Hint
instance SearcherData Hint where
    text = to $ \case
        Command h -> h ^. text
        Node    h -> h ^. text
        Pipe    p -> pack . show $ p

instance SearcherHint Hint where
    prefix = to $ \case
        Command h -> h ^. prefix
        Node    h -> h ^. prefix
        Pipe    p -> "pipePre"
    documentation = to $ \case
        Command h -> h ^. documentation
        Node    h -> h ^. documentation
        Pipe    h -> "pipeDoc"
