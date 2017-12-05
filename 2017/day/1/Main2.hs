module Main where

import Data.Char (digitToInt)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- captcha :: ByteString -> Int
captcha s =
    let n = B.length s
        (s', t') = B.splitAt (n `div` 2) s
        t = B.append t' s'
    in sum $ B.zipWith (\x y -> if x == y then digitToInt x else 0) s t

main = B.readFile "input.txt" >>= mapM_ (print . captcha) . B.lines
