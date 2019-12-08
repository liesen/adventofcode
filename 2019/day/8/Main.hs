import Data.Ord
import Data.List
import Data.List.Split

main = do
    input <- head . lines <$> readFile "input.txt"
    let (width, height) = (25, 6)

    -- Part 1
    let layer0 = minimumBy (comparing (length . takeWhile (== '0') . sort)) (chunksOf (width * height) input)
    print $ length (filter (== '1') layer0) * length (filter (== '2') layer0)
