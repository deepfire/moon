{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Text.Zipper.Extra
where

import Prelude hiding (all, dropWhile, last, length, null)

import Data.Char
import Data.Function ((&))
import Data.Text
import Data.Text.Extra
import Data.Text.Zipper

import Debug.TraceErr (traceErr)
import Text.Printf (printf)


lookCharLeft :: TextZipper -> Maybe Char
lookCharLeft TextZipper{..}
  | _textZipper_before == "" =
    if _textZipper_linesBefore == [] then Nothing else Just '\n'
  | otherwise = Just $ Data.Text.last _textZipper_before

lookCharRight :: TextZipper -> Maybe Char
lookCharRight TextZipper{..}
  | _textZipper_after == "" =
    if _textZipper_linesAfter == [] then Nothing else Just '\n'
  | otherwise = Just $ Data.Text.head _textZipper_after

-- | Move the cursor at the start of the previous word, if possible
leftWord :: TextZipper -> TextZipper
leftWord z@(TextZipper _ b _ _) =
  let firstAlnum    = seekTextLeft        isAlpha  b (length b - 1)
      firstBoundary = seekTextLeft (not . isAlpha) b firstAlnum
  in z & if
    | null b                    -> id
    | firstBoundary == firstAlnum -> home
    | otherwise                   -> leftN (length b - 1 - firstBoundary)

-- | Move the cursor right by one word, if possible
rightWord :: TextZipper -> TextZipper
rightWord z@(TextZipper _ _ a _) =
  let firstAlnum    = seekTextRight        isAlpha  a 0
      firstBoundary = seekTextRight (not . isAlpha) a firstAlnum
  in z & if
    | null a                    -> id
    | firstBoundary == firstAlnum -> end
    | otherwise                   -> rightN firstBoundary

-- | Delete a word to the right of the cursor. Deletes all whitespace until it
-- finds a non-whitespace character, and then deletes contiguous non-whitespace
-- characters.
deleteRightWord :: TextZipper -> TextZipper
deleteRightWord (TextZipper lb b a la) =
  let a' = dropWhile isSpace a
  in  if null a'
        then case la of
          [] -> TextZipper lb b a' []
          (l:ls) -> deleteRightWord $ TextZipper lb b l ls
        else TextZipper lb b (dropWhile (not . isSpace) a') la

killLine :: TextZipper -> TextZipper
killLine (TextZipper lb b _ la) = TextZipper lb b "" la

complete :: Char -> Maybe Text -> TextZipper -> TextZipper
complete compChar mText tz@(TextZipper _ b _ _) =
  let lastW = lastWord b
      nonNull = not (null lastW)
      lastCompletible = nonNull && all isAlphaNum (lastWord b)
      lastIsSpace     = nonNull && last b == compChar
  in case (lastCompletible, lastIsSpace || not nonNull, mText) of
    (True,  False, Just x) ->
      case ( lastW `isPrefixOf` x
           , commonPrefixes x (takeEnd (lastWordLen b) b)
           ) of
        (True,  Nothing)         -> insert   (x  `snoc` compChar) tz
        (True,  Just (_, tl, _)) -> insert   (tl `snoc` compChar) tz
        (False, _)               -> deleteLeftWord tz
                                    & insert (x  `snoc` compChar)
    (_,     True,  Just x) -> insert (x  `snoc` compChar) tz
    _                      -> insertChar compChar tz
