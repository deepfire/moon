{-# LANGUAGE ScopedTypeVariables        #-}

module Moon.CLI (main) where

import           Codec.Serialise
import           Data.Binary
import qualified Data.ByteString.Lazy           as LBS
import qualified Network.WebSockets             as WS
import           Options.Applicative
import           Options.Applicative.Common

import Moon.Face
import Moon.Face.Haskell
import Moon.Lift hiding (main)

main :: IO ()
main = do
  req <- execParser $ (info $ (optional parseHaskellRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      WS.sendBinaryData conn (serialise $ case req :: Maybe SomeHaskellRequest of
                                 Nothing -> End
                                 Just x  -> BS $ LBS.toStrict $ serialise x)
      reply :: WSMesg <- deserialise <$> WS.receiveData conn
      WS.sendBinaryData conn (serialise $ End)
      putStrLn $ case reply of
        BS bs -> (show :: HaskellReply -> String) $ deserialise $ LBS.fromStrict bs
        End   -> "received End"
      pure ()
