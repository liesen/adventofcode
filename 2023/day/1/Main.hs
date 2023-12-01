{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Internal.Search qualified as Text

part1, part2 :: [(Text, Int)]
part1 = zip (map Text.singleton ['1' .. '9']) [1 .. 9]
part2 = part1 ++ zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 .. 9]

calibrationValue digits haystack = first * 10 + last
  where
    locs = concatMap (\(digit, value) -> map (,value) (Text.indices digit haystack)) digits
    (_, first) = minimum locs
    (_, last) = maximum locs

main = do
  input <- Text.readFile "input"
  let lines = Text.lines input

  print $ sum $ map (calibrationValue part1) lines
  print $ sum $ map (calibrationValue part2) lines