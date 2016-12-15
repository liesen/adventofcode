{-# LANGUAGE ViewPatterns #-}
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import Control.Parallel.Strategies

--- Day 14: One-Time Pad ---

md5 :: String -> Int -> String
md5 input n = show digest
  where digest :: Digest MD5
        digest = hash (C.concat [C.pack input, C.pack (show n)])

md5s :: String -> [String]
md5s input = map (md5 input) [0..]

contains3 (x:y:z:zs) | x == y && y == z = x : contains3 (y:z:zs)
                     | otherwise        = contains3 (y:z:zs)
contains3 _          = []

-- contains5 x zs = or (elem (replicate 5 x) . window 5) zs
contains5 :: Char -> [String] -> Bool
contains5 x zs = or [replicate 5 x `elem` window 5 z | z <- zs]

window :: Int -> [a] -> [[a]]
window m = foldr (zipWith (:)) (repeat []) . take m . tails

solutions = mapMaybe f $ window 1001 $ zip [0..] $ md5s "ahsbgdzn"
  where
    -- If hash contains multiple triples, use only the first
    -- f ((i, contains3 -> xs):(map snd -> ys)) | any (flip contains5 ys) xs = Just i
    f ((i, contains3 -> x:_):(contains5 x . map snd -> True)) = Just i
    f _ = Nothing

main1 = print (solutions !! 63)

--- Part Two ---

md5_2 = show . digest
  where
    digest :: String -> Digest MD5
    digest = hash . C.pack

stretch 0 x = x
stretch n x = stretch (n - 1) (md5_2 x)

solutions2 = mapMaybe f $ window 1001 $ zip [0..] $ map (stretch 2016) $ md5s "ahsbgdzn"
  where
    f ((i, contains3 -> x:_):(contains5 x . map snd -> True)) = Just i
    f _ = Nothing

main2 = print (solutions2 !! 63)

main = main1 >> main2
