import qualified Data.Vector as Vector
import Data.Vector (Vector, (!))

part1 :: Int -> Vector Int -> Maybe Int
part1 windowSize a = go windowSize
  where
    go k | k >= Vector.length a = Nothing
         | null [() | i <- [k - windowSize..k - 2], j <- [i + 1..k - 1], a ! i + a ! j == a ! k] = Just (a ! k)
         | otherwise = go (k + 1)

part2 :: Int -> Vector Int -> Int
part2 target a = go 0 0 0
  where
    go acc lo hi
        | acc == target =
            let slice = Vector.take (hi - lo) (Vector.drop lo a)
            in Vector.minimum slice + Vector.maximum slice
        | acc < target = go (acc + a ! hi) lo (hi + 1)
        | acc > target = go (acc - a ! lo) (lo + 1) hi

main = do
    input <- readFile "input.txt"
    let numbers = Vector.fromList $ map read $ lines input

    -- Part 2
    let Just ans1 = part1 25 numbers
    print ans1

    -- Part 1
    print $ part2 ans1 numbers