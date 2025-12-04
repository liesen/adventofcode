{-# LANGUAGE MultilineStrings #-}

import Control.Monad
import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.List (maximum, maximumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord (compare)

input =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111
  """

-- Left-biased version of https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.Data.Foldable.html#maximumBy
maximumBy' :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy' cmp =
  fromMaybe (errorWithoutStackTrace "maximumBy: empty structure")
    . foldl' max' Nothing
  where
    max' mx y =
      Just $! case mx of
        Nothing -> y
        Just x -> case cmp x y of
          LT -> y
          _ -> x -- GT, EQ

joltage2 :: [Int] -> Int
joltage2 bank =
  let (i, x) = maximumBy' (compare `on` snd) (zip [0 ..] bank)
   in case splitAt (i + 1) bank of
        -- The largest digit is the last
        (xs, []) -> maximum (take i bank) * 10 + x
        (xs, ys) -> x * 10 + maximum ys

joltage :: Int -> [Int] -> [Int]
joltage n bank = go n (length bank) bank
  where
    go 1 m xs = [maximum xs]
    go n m (x : xs)
      | m == n = x : xs
      | otherwise = max (x : go (n - 1) (m - 1) xs) (go n (m - 1) xs)

memoJoltage :: Int -> [Int] -> [Int]
memoJoltage n bank = fst (go mempty n (length bank) bank)
  where
    go memo n m (x : xs) =
      case Map.lookup (n, m) memo of
        Just ans -> (ans, memo)
        Nothing ->
          if n == m
            -- Must use rest of the digits
            then (x : xs, Map.insert (n, m) (x : xs) memo)
            else case n of
              1 ->
                -- Must use the largest of the digits
                let ans = [maximum (x : xs)]
                 in (ans, Map.insert (n, m) ans memo)
              _ ->
                let (xs', memo') = go memo (n - 1) (m - 1) xs
                    (xs'', memo'') = go memo' n (m - 1) xs
                    ans = max (x : xs') xs''
                 in (ans, Map.insert (n, m) ans memo'')

main = do
  input <- getContents

  -- Part 1
  print $ sum $ map (joltage2 . map digitToInt) $ lines input

  -- Part 2
  print $ sum $ map (read . map intToDigit . memoJoltage 12 . map digitToInt) $ lines input
