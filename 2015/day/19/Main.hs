{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQueue

newtype Replacement a = Replacement (a, a) deriving (Eq, Ord)

instance Show a => Show (Replacement a) where
    show (Replacement (x, y)) = show x ++ " => " ++ show y

parseReplacement (span isAlpha -> (x, ' ':'=':'>':' ':(span isAlpha -> (y, z)))) = Replacement (x, y)

invert (Replacement (x, y)) = Replacement (reverse y, reverse x)

replace (Replacement (needle, z)) haystack =
    [ x ++ z ++ drop (length needle) y
    | (x, y) <- zip (inits haystack) (tails haystack)
    , needle `isPrefixOf` y
    ]

next :: (Replacement a -> a -> [a]) -> [Replacement a] -> a -> [(a, Int)]
next f rs x = map (\x' -> (x', 1)) $ concatMap (\r -> f r x) rs

astar :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> (a -> Int) -> a -> [(a, Int)]
astar rep next heuristic start = loop Set.empty (PQueue.singleton 0 (start, 0))
  where
    loop _    (PQueue.minView -> Nothing) = []
    loop seen (PQueue.minView -> Just ((x, cost), q1))
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = foldl' (\q (x', stepcost) -> let cost' = cost + stepcost in cost' `seq` PQueue.insert (cost' + heuristic x') (x', cost') q) q1 (next x)

main = do
    input <- readFile "input.txt"
    let (map parseReplacement -> replacements, "":molecule:[]) = break null (lines input)
    
    -- Part 1
    print $ length $ nub $ concatMap (\r -> replace r molecule) replacements

    -- Part 2
    -- Do replacements in reverse to avoid backtracking
    let revReplacements = map invert replacements
        revMolecule = reverse molecule
    let Just (_e, ans) = find ((== "e") . fst) $ astar id (next replace revReplacements) length revMolecule
    print ans
