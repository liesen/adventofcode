import Data.List (sort)
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

part1 jolts = 
  let deltas = zipWith (-) (tail jolts) jolts
  in length (filter (== 1) deltas) * length (filter (== 3) deltas)

part2 jolts = IntMap.lookup (last jolts) a
  where
    a = IntMap.fromListWith (+) [(i, f i) | i <- jolts]

    f 0 = 1
    f i = IntMap.findWithDefault 0 (i - 3) a
        + IntMap.findWithDefault 0 (i - 2) a
        + IntMap.findWithDefault 0 (i - 1) a

main = do
    input <- readFile "input.txt"
    let adapters = map read (lines input)
        jolts = 0 : sort adapters ++ [maximum adapters + 3]

    -- Part 1
    print $ part1 jolts

    -- Part 2
    print $ fromJust $ part2 jolts