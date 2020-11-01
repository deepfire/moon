module Dom.Error (module Dom.Error) where

import Codec.Serialise
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra
import Data.Text
import Data.String


--------------------------------------------------------------------------------
-- * Error
--
newtype Error =
  Error { showError :: Text }
  deriving (Eq, Serialise)

instance Show Error where
  show x = "#<ERROR: " <> unpack (showError x) <> ">"

instance IsString Error where
  fromString = Error . pack

mkError :: String -> Error
mkError = Error . pack

mapError :: (Text -> Text) -> Error -> Error
mapError f (Error t) = Error (f t)
{-# INLINE mapError #-}

qualError :: Text -> Error -> Error
qualError q = mapError ((q <> ": ") <>)
{-# INLINE qualError #-}

--------------------------------------------------------------------------------
-- * Fallible
--
type Fallible x = Either Error x

fall :: Text -> Fallible x
fall = Left . Error

fallS :: String -> Fallible x
fallS = Left . mkError

fallShow :: Show a => a -> Fallible b
fallShow = Left . Error . pack . show

fallDesc :: Text -> Text -> Fallible b
fallDesc desc = Left . Error . (desc <>) . (": " <>)

fallDescShow :: Show a => Text -> a -> Fallible b
fallDescShow desc = Left . Error . (desc <>) . (": " <>) . pack . show

fallM :: Monad m => Text -> m (Fallible x)
fallM = pure . fall

mapFall :: (Error -> Error) -> Fallible x -> Fallible x
mapFall f = \case
  Left e ->  Left (f e)
  Right x -> Right x
{-# INLINE mapFall #-}

qualExceptErr :: Functor m => Text -> ExceptT Error m a -> ExceptT Error m a
qualExceptErr t = firstExceptT (qualError t)
{-# INLINE qualExceptErr #-}

newQualExceptErr :: Text -> IO (Fallible a) -> ExceptT Error IO a
newQualExceptErr t = qualExceptErr t . newExceptT
{-# INLINE newQualExceptErr #-}

