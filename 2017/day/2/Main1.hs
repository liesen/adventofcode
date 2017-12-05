module Main where

checksum :: String -> Integer
checksum = sum . map minmax . map (map read . words) . lines
  where minmax xs = maximum xs - minimum xs

main = readFile "input.txt" >>= print . checksum
