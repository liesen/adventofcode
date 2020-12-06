import Data.List
import qualified Data.IntSet as IntSet

data Range a = Range !a !a
    deriving (Eq, Ord, Show)

upperHalf (Range lo hi) = Range ((lo + hi + 1) `div` 2) hi
lowerHalf (Range lo hi) = Range lo (lo + (hi - lo) `div` 2)

data Seat a = Seat (Range a) (Range a)
    deriving (Eq, Ord, Show)

step (Seat r c) 'B' = Seat (upperHalf r) c
step (Seat r c) 'F' = Seat (lowerHalf r) c
step (Seat r c) 'R' = Seat r (upperHalf c)
step (Seat r c) 'L' = Seat r (lowerHalf c)

scan :: String -> Seat Int
scan = foldl step (Seat (Range 0 127) (Range 0 7))

seatId :: Seat Int -> Int
seatId (Seat (Range r _) (Range c _)) = r * 8 + c

main = do
    input <- readFile "input.txt"
    let seats = map scan (lines input)
        seatIds = IntSet.fromList (map seatId seats)

    -- Part 1
    let Just (maxSeatId, _) = IntSet.maxView seatIds
    print maxSeatId

    -- Part 2
    let Just (minSeatId, _) = IntSet.minView seatIds
        Just yourSeatId = find (`IntSet.notMember` seatIds) [minSeatId..maxSeatId]
    print yourSeatId