import Data.Monoid
import Data.Traversable
import qualified Data.Set as Set


readSigned ('+':s) = read s
readSigned ('-':s) = -read s

main = do
    input <- readFile "input.txt"
    let changes = map readSigned (lines input)

    -- Part 1
    print $ sum changes

    -- Part 2
    print $ head . concat . snd $ mapAccumL (\xs x -> (Set.insert x xs, if x `Set.member` xs then [x] else [])) Set.empty $ scanl1 (+) $ cycle changes
