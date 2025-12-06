{-# LANGUAGE MultilineStrings #-}

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

joltage :: Int -> Array Int Int -> Maybe Integer
joltage n bank = getMax <$> (memoTable ! (n, 0))
  where
    (_, maxIndex) = bounds bank
    bankSize = maxIndex + 1
    bnds = ((0, 0), (n, bankSize))

    memoTable :: Array (Int, Int) (Maybe (Max Integer))
    memoTable = array bnds [(ki, subproblem ki) | ki <- range bnds]

    subproblem :: (Int, Int) -> Maybe (Max Integer)
    subproblem (k, i) -- k digits left to pick, starting at index i
      | k == 0 = Just (Max 0) -- All digits picked
      | i >= bankSize || (bankSize - i) < k = Nothing -- No digits left to pick from
      | otherwise =
          let currentDigit = fromIntegral (bank ! i)
              prepend digit (Max val) = Max (digit * (10 ^ (k - 1)) + val)
              pick = prepend currentDigit <$> memoTable ! (k - 1, i + 1)
              skip = memoTable ! (k, i + 1)
           in pick <> skip

main = do
  input <- getContents
  let parse s = listArray (0, length s - 1) (map digitToInt s)
      banks = map parse (lines input)

  -- Part 1
  print $ sum $ map (fromJust . joltage 2) banks

  -- Part 2
  print $ sum $ map (fromJust . joltage 12) banks
