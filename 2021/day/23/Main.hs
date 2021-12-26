{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}
import Control.Monad
import Data.Array (Array, Ix, (!), (//))
import Data.Array qualified as Array
import Data.List
import Data.Monoid
import Data.Maybe
import Debug.Trace

import Data.PQueue.Prio.Min qualified as PQueue
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set


astar :: Ord r
       => (a -> r)  -- ^ Representation
       -> (a -> [(a, Int)])  -- ^ Neighbor function (neighbor node, step cost)
       -> (a -> Int)  -- ^ Heuristic function (if const 0 then astar = bfs)
       -> [a]  -- ^ Start nodes
       -> [(a, Int)]
astar rep next heur starts = loop Set.empty q0
  where
    q0 = PQueue.fromList $ map (\start -> (heur start, (start, 0))) starts
    loop _    (PQueue.minView -> Nothing) = []
    loop seen (PQueue.minView -> Just ((x, cost), q1))
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = foldl' (\q (x', stepcost) -> let cost' = cost + stepcost
                                            in cost' `seq` PQueue.insert (cost' + heur x') (x', cost') q)
                      q1
                      (next x)


-- TODO can this be elegantly solved with monoid magic?


{-
 0123456789A
#############
#...........#  <- hallway
###B#C#B#D###
  #A#D#C#A#    <- rooms
  #########
   ^ ^ ^ ^
   2 4 6 8
-}

data Amphipod = A | B | C | D deriving (Eq, Ix, Ord, Show)

type Hallway = Array Int (Maybe Amphipod)

type Rooms = Array Amphipod (Maybe Amphipod, Maybe Amphipod)

data Burrow = Burrow Hallway Rooms
  deriving (Eq, Ord)

start A = 2
start B = 4
start C = 6
start D = 8

stepEnergy A = 1
stepEnergy B = 10
stepEnergy C = 100
stepEnergy D = 1000

instance Show Burrow where
    show (Burrow hallway rooms) = 
      unlines [ replicate 13 '#'
              , "#" ++ concatMap (maybe "." show) (Array.elems hallway) ++ "#"
              , "###" ++ concat (intersperse "#" [maybe "." show top | (top, _) <- Array.elems rooms]) ++ "###"
              , "  #" ++ concat (intersperse "#" [maybe "." show bot | (_, bot) <- Array.elems rooms]) ++ "#"
              , "  #" ++ replicate 8 '#'
              ]

done (Burrow hallway rooms) =
    and [ a == Just x && b == Just x | (x, (a, b)) <- Array.assocs rooms ]

-- Heuristic: number of amphipods not in the correct room
heur (Burrow hallway rooms) =
    sum [1 | (room, (a, b)) <- Array.assocs rooms, c <- [a, b], c /= Just room]

next burrow = moveIntoRooms burrow ++ moveIntoHallway burrow

-- Move amphipod in rooms out into the hallway
moveIntoHallway (Burrow hallway rooms) = do
    (room, (a, b)) <- Array.assocs rooms
    let h = start room -- Hallway index of room
    -- Find open positions in the hallway
    let left = takeWhile (vacant . snd) $ filter ((`notElem` [2, 4, 6, 8]) . snd) $ zip [1..] (reverse [0..h - 1])
    let right = takeWhile (vacant . snd) $ filter ((`notElem` [2, 4, 6, 8]) . snd) $ zip [1..] [h + 1..10]
    (numSteps, j) <- left ++ right

    case (a, b) of
        (Just x, _) -> do
          let stepCost = stepEnergy x * (numSteps + 1)
          return (Burrow (hallway // [(j, a)]) (rooms // [(room, (Nothing, b))]), stepCost)
        (Nothing, Just y) | y == room -> fail "locked in place"
        (Nothing, Just y) | y /= room -> do
          let stepCost = stepEnergy y * (numSteps + 2)
          return (Burrow (hallway // [(j, b)]) (rooms // [(room, (Nothing, Nothing))]), stepCost)
        (Nothing, Nothing) -> fail "room empty"
  where
    vacant i = isNothing (hallway ! i)

-- Move amphipods from hallway into rooms
moveIntoRooms (Burrow hallway rooms) = do
    (i, Just x) <- Array.assocs hallway

    -- Check that the hallway is clear 
    let steps = if i < start x then [i + 1..start x] else [i - 1, i - 2..start x]
        numSteps = length steps
    guard $ all (\j -> isNothing (hallway ! j)) steps
    
    let hallway' = hallway // [(i, Nothing)]

    -- Check that the amphipod's room is able to move into
    case rooms ! x of
        (Just _, _) -> fail "room is not vacant"
        (Nothing, Just y) | y == x -> do
            let stepCost = stepEnergy x * (numSteps + 1)
            return (Burrow hallway' (rooms // [(x, (Just x, Just y))]), stepCost)
        (Nothing, Just y) | y /= x ->
            fail "there's an alien amphipod in x's room"
        (Nothing, Nothing) -> do
            let stepCost = stepEnergy x * (numSteps + 2)
            return (Burrow hallway' (rooms // [(x, (Nothing, Just x))]), stepCost)

input = Burrow hallway rooms
  where
    hallway = Array.listArray (0, 10) (repeat Nothing)
    rooms = Array.array (A, D) [(A, (Just D, Just B)), (B, (Just B, Just D)), (C, (Just A, Just A)), (D, (Just C, Just C))]

main = do
    -- Part 1
    case find (done . fst) (astar id next heur [input]) of
        Just (_, ans) -> print ans
