{-# LANGUAGE ViewPatterns #-}
import Data.Char

hash :: String -> Int
hash = foldl f 0
    where f cur ch = ((cur + ord ch) * 17) `mod` 256

split [] = []
split xs = case span (/= ',') xs of
    (y, ',':ys) -> y : split ys
    (y, [])     -> [takeWhile (not . isSpace) y]

main = do
    input <- readFile "input"
    print $ sum $ map hash $ split input