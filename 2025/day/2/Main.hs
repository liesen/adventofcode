{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Ix (range)
import Data.Char (isDigit)
import Data.List (isPrefixOf, splitAt)

input =
  concat $
    lines
      """
      11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
      1698522-1698528,446443-446449,38593856-38593862,565653-565659,
      824824821-824824827,2121212118-2121212124
      """

parseRange (break (== '-') -> (read -> start, '-' : (read -> end))) = (start, end)

parseRanges (break (== ',') -> (range, "")) = [parseRange range]
parseRanges (break (== ',') -> (range, ',' : rest)) = parseRange range : parseRanges rest

-- Find lengths of repeating sequences for a number
findRepeating :: Int -> [Int]
findRepeating n = [i | i <- [1 .. numDigits `div` 2], isRepeating i s]
  where
    s = show n
    numDigits = length s

-- Checks if a number is made up of repeating sequences of length @w@
isRepeating :: Int -> String -> Bool
isRepeating w (splitAt w -> (xs, [])) = True
isRepeating w (splitAt w -> (xs, ys)) = xs `isPrefixOf` ys && isRepeating w ys

main = do
  input <- getContents
  let ids = concatMap range (parseRanges input)

  -- Part 1
  print $
    sum $
      filter
        ( \n ->
            let numDigits = length (show n)
             in even numDigits && (numDigits `div` 2) `elem` findRepeating n
        )
        ids

  -- Part 2
  print $ sum $ filter (not . null . findRepeating) ids
