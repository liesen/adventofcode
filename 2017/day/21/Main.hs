{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Maybe
import Data.Monoid
import Data.Array
import Data.List

newtype Im = Im (Array (Int, Int) Char) deriving (Eq, Ord)

instance Show Im where
  show (Im im) = unlines [[im ! (i, j) | j <- [0..n]] | i <- [0..n]]
    where
      bnds@(_, (n, _)) = bounds im

image0 = Im $ listArray ((0, 0), (2, 2)) ".#...####"

transform :: (Int -> (Int, Int) -> (Int, Int)) -> Im -> Im
transform f (Im im) = Im $ ixmap bnds (f size) im
  where
    bnds@(_, (n, _)) = bounds im
    size = n + 1

rotateL = transform $ \n (i, j) -> (j, i)
mirrorV = transform $ \n (i, j) -> (n - i - 1, j)
mirrorH = transform $ \n (i, j) -> (i, n - j - 1)

patterns :: Im -> [Im]
patterns im = map ($ im) $ [
    id,
    mirrorH,
    mirrorV,
    mirrorH . mirrorV,
    rotateL,
    rotateL . mirrorH,
    rotateL . mirrorV,
    rotateL . mirrorH . mirrorV
  ]

ruleKey :: Im -> String
ruleKey (Im im) = concat $ intersperse "/" [[im ! (i, j) | j <- [0..n]] | i <- [0..n]]
  where
    bnds@(_, (n, _)) = bounds im

testRules = [
    ("../.#", "##./#../..."),
    (".#./..#/###", "#..#/..../..../#..#")
  ]

parseKey :: String -> Im
parseKey s =
    let es = f (0, 0) s
        (n, m) = last (map fst es)
    in Im $ array ((0, 0), (n, m)) es
  where
    f (i, j) []       = []
    f (i, j) ('/':xs) = f (0, j + 1) xs
    f (i, j) (x:xs)   = ((i, j), x) : f (i + 1, j) xs

applyRule rules = parseKey . fromJust . getFirst . mconcat . map (First . flip lookup rules . ruleKey) . patterns

enhance rules (Im im)
    | size `mod` 2 == 0 = join 3 $ map (applyRule rules) $ divide 2 (Im im)
    | size `mod` 3 == 0 = join 4 $ map (applyRule rules) $ divide 3 (Im im)
  where
    bnds@(_, (n, _)) = bounds im
    size = n + 1

divide q (Im im) = do
    bnds@((i', j'), _) <- [((i, j), (i + q - 1, j + q - 1)) | i <- [0, q..n - 1], j <- [0, q..n - 1]]
    let im' = ixmap ((0, 0), (q - 1, q - 1)) (\(i, j) -> (i + i', j + j')) im
    return $ Im im'
  where
    (_, (n, _)) = bounds im

join :: Int -> [Im] -> Im
join q ims = Im $ array ((0, 0), (size - 1, size - 1)) $ concat $ zipWith translate [(i, j) | i <- [0, q..size - 1], j <- [0, q..size - 1]] ims
  where
    k = truncate (sqrt (fromIntegral (length ims)))
    size = q * k
    translate (di, dj) (Im im) = map (\((i, j), x) -> ((i + di, j + dj), x)) $ assocs im

parseRules = map parseRule . lines
  where
    parseRule (break (== ' ') -> (key, ' ':'=':'>':' ':rule)) = (key, rule)

main = do
    input <- readFile "input.txt"
    let rules = parseRules input

    -- Part 1
    let (Im x) = iterate (enhance rules) image0 !! 5
    print $ length . filter (== '#') $ elems x

    -- Part 2
    let (Im x) = iterate (enhance rules) image0 !! 18
    print $ length . filter (== '#') $ elems x
