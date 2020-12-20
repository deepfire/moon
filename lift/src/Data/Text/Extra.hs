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

lastWordLen :: (Char -> Bool) -> Text -> Int
lastWordLen constituency x =
  let firstAlnum    = seekTextLeft        constituency  x (length x)
      firstBoundary = seekTextLeft (not . constituency) x firstAlnum
  in if
    | null x                    -> 0
    | firstBoundary == firstAlnum -> length x
    | otherwise                   -> length x - 1 - firstBoundary

lastWord :: (Char -> Bool) -> Text -> Text
lastWord constituency x = takeEnd (lastWordLen constituency x) x
