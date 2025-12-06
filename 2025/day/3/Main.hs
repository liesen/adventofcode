{-# LANGUAGE MultilineStrings #-}

import Control.Monad
import Data.Array
import Data.Char (digitToInt, intToDigit)
import Data.Maybe
import Data.Semigroup (Max (..))

input =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111
  """

joltage :: Int -> Array Int Int -> Maybe Int
joltage n bank = getMax <$> (memoTable ! (n, 0))
  where
    (_, maxIndex) = bounds bank
    bankSize = maxIndex + 1
    bnds = ((0, 0), (n, bankSize))

    memoTable :: Array (Int, Int) (Maybe (Max Int))
    memoTable = array bnds [(k, go k) | k <- range bnds]

    go :: (Int, Int) -> Maybe (Max Int)
    go (k, i)
      -- Done selecting, the resulting number is 0. Success is Just (Max 0)
      | k == 0 = Just (Max 0)
      -- Insufficient digits remaining
      | i >= bankSize || (bankSize - i) < k = Nothing
      -- Recurse
      | otherwise =
          let concat digit (Max val) = Max (digit * (10 ^ (k - 1)) + val)
              -- Take the current digit
              take = concat (bank ! i) <$> memoTable ! (k - 1, i + 1)
              -- Skip the current digit
              skip = memoTable ! (k, i + 1)
           in take <> skip

main = do
  input <- getContents
  let parseBank s = listArray (0, length s - 1) (map digitToInt s)
      banks = map parseBank (lines input)

  -- Part 1
  print $ sum $ map (fromJust . joltage 2) banks

  -- Part 2
  print $ sum $ map (fromJust . joltage 12) banks
