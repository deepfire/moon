{-# LANGUAGE MultiWayIf #-}

module Data.Text.Extra
where

import Prelude hiding (length, null)

import Data.Char
import Data.Text


textTest :: (Int -> Bool) -> (Char -> Bool) -> Text -> Int -> Bool
textTest limit f text (limit -> False) = True
textTest limit f text x = f $ index text x

seekTextLeft :: (Char -> Bool) -> Text -> Int -> Int
seekTextLeft test text start =
  until (textTest limit test text) (subtract 1) from
 where
   limit = (>= 0)
   from = start `min` length text - 1

seekTextRight :: (Char -> Bool) -> Text -> Int -> Int
seekTextRight test text start =
  until (textTest limit test text) (+ 1) from
 where
   limit = (< length text)
   from = start `max` 0

lastWordLen :: Text -> Int
lastWordLen x =
  let firstAlnum    = seekTextLeft        isAlpha  x (length x - 1)
      firstBoundary = seekTextLeft (not . isAlpha) x firstAlnum
  in if
    | null x                    -> 0
    | firstBoundary == firstAlnum -> length x
    | otherwise                   -> length x - 1 - firstBoundary

lastWord :: Text -> Text
lastWord x = takeEnd (lastWordLen x) x
