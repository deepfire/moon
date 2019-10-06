{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import qualified JavaScript.WebSockets            as WS
import qualified Data.ByteString                  as BS

main :: IO ()
main =
  WS.withUrl "ws://127.0.0.1:29671" $ \conn -> do
    WS.send conn (BS.pack [0, 2, 4, 8, 16, 32, 64])
    x <- WS.receive conn
    putStrLn (show x)
