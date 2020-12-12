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

substIfNot :: (Char -> Bool) -> Char -> Text -> Text
substIfNot f to xs = Data.Text.map (\c -> if f c then c else to) xs
{-# INLINE substIfNot #-}

complete :: (Char -> Bool) -> Char -> Maybe Text -> TextZipper -> TextZipper
complete constituency
         compChar
         mCompleteTo
         tz@(TextZipper _ b _ _) =
  let lastW = lastWord b
      lastWcompletible = flip all lastW
        \x -> constituency x || x == '.'
  in
      (traceErr $ mconcat $
        [ "complete: "
        , "lastW=", show lastW, ", "
        , "completible=", show lastWcompletible, ", "
        , "mCompleteTo=", show mCompleteTo, ", "
        ]) $
    case (lastWcompletible, mCompleteTo) of
      (True, Just completeTo) ->
        let lastWisPrefix = lastW `isPrefixOf` completeTo
            commons = commonPrefixes completeTo (takeEnd (lastWordLen b) b)
        in
          (traceErr $ mconcat $
           [ "complete: "
           , "lastWisPrefix=", show lastWisPrefix, ", "
           , "commons=", show commons, ", "
           ]) $
        case (lastWisPrefix, commons) of
          (True,  Nothing)         -> insert completeTo tz
          (True,  Just (_, tl, _)) -> insert tl tz
          (False, _)               -> deleteLeftWord tz
                                      & insert completeTo
      (_,   Just completeTo)       -> insert completeTo tz
      _                            -> insertChar compChar tz
