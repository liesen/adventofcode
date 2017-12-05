module Main where

import Data.Char (digitToInt)

captcha (x:xs) = sum . map (digitToInt . fst) . filter (uncurry (==)) $ zip (x:xs) (xs ++ [x])

main = readFile "input.txt" >>= mapM_ (print . captcha) . lines
