module Pipe
  ( module Basis
  , module Type
  , module SomeType
  , module SomeValue
  --
  , module Ground.Expr
  --
  , module Pipe.Ops
  , module Pipe.Scope
  , module Pipe.Space
  , module Pipe.Types
  )
where

import Basis
import SomeType
import SomeValue

import Ground.Expr

import Pipe.Ops
import Pipe.Scope
import Pipe.Space
import Pipe.Types

-- * Attic of exciting unattributables
--
-- parseSomePipe :: (QName Pipe -> Maybe (SomePipe ())) -> Opt.Parser (SomePipe ())
-- parseSomePipe lookupPipe =
--   runA $ proc () -> do
--     p <- asA (strArgument (metavar "PIPEDESC")) -< ()
--     returnA -< case do
--       ast <- parse p
--       compile opsDesc lookupPipe ast of
--       Left  e -> error (unpack e)
--       Right x -> x
