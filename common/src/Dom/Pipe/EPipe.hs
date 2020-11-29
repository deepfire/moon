module Dom.Pipe.EPipe (module Dom.Pipe.EPipe) where

import Codec.Serialise
import Data.String (IsString(..))
import GHC.Generics (Generic)

import Dom.Error
import Dom.SomeType

import Basis


--------------------------------------------------------------------------------
-- * EPipe
--
data ErrPipe a
  = EParse     { unEPipe :: !a }
  | EAnal      { unEPipe :: !a }
  | EName      { unEPipe :: !a }
  | ECompile   { unEPipe :: !a }

  | EUnsat     { unEPipe :: !a, epArgs :: [SomeType], epOut :: SomeType }
  | ENonGround { unEPipe :: !a }

  | EApply     { unEPipe :: !a }
  | ETrav      { unEPipe :: !a }
  | EComp      { unEPipe :: !a }

  | EType      { unEPipe :: !a }
  | EExec      { unEPipe :: !a }
  deriving (Functor, Eq, Generic, Show)

instance Serialise a => Serialise (ErrPipe a)

type EPipe = ErrPipe Error

type PFallible = Either EPipe

instance IsString EPipe where
  fromString = \case
    'E':xs -> case xs of
      'P':'a':'r':'s':'e':        ':':' ':s -> EParse   . Error . pack $ s
      'A':'n':'a':'l':            ':':' ':s -> EAnal    . Error . pack $ s
      'N':'a':'m':'e':            ':':' ':s -> EName    . Error . pack $ s
      'C':'o':'m':'p':'i':'l':'e':':':' ':s -> ECompile . Error . pack $ s

      'A':'p':'p':'l':'y':        ':':' ':s -> EApply   . Error . pack $ s
      'T':'r':'a':'v':            ':':' ':s -> ETrav    . Error . pack $ s
      'C':'o':'m':'p':            ':':' ':s -> EComp    . Error . pack $ s

      'T':'y':'p':'e':            ':':' ':s -> EType    . Error . pack $ s
      'E':'x':'e':'c':            ':':' ':s -> EExec    . Error . pack $ s
      e -> error $ "Invalid IsString encoding of EPipe: " <> e
    e -> error $ "Invalid IsString encoding of EPipe: " <> e


newPipeExceptErr :: (Error -> EPipe) -> IO (Fallible a) -> ExceptT EPipe IO a
newPipeExceptErr inj act = newExceptT $ left inj <$> act
{-# INLINE newPipeExceptErr #-}
