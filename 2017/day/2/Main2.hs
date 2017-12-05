module Main2 where

checksum :: String -> Integer
checksum = sum . map (head . divisibles) . map (map read . words) . lines
  where divisibles xs = [q | x <- xs, y <- xs, x /= y, let (q, r) = max x y `quotRem` min x y, r == 0]

main = readFile "input.txt" >>= print . checksum
