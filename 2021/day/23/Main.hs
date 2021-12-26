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

type Rooms1 = Array Amphipod (Maybe Amphipod, Maybe Amphipod)

data Burrow1 = Burrow1 Hallway Rooms1 deriving (Eq, Ord)

type Rooms2 = Array Amphipod (Maybe Amphipod, Maybe Amphipod, Maybe Amphipod, Maybe Amphipod)

data Burrow2 = Burrow2 Hallway Rooms2 deriving (Eq, Ord)

start A = 2
start B = 4
start C = 6
start D = 8

stepEnergy A = 1
stepEnergy B = 10
stepEnergy C = 100
stepEnergy D = 1000

instance Show Burrow1 where
    show (Burrow1 hallway rooms) = 
      unlines [ replicate 13 '#'
              , "#" ++ concatMap (maybe "." show) (Array.elems hallway) ++ "#"
              , "###" ++ concat (intersperse "#" [maybe "." show top | (top, _) <- Array.elems rooms]) ++ "###"
              , "  #" ++ concat (intersperse "#" [maybe "." show bot | (_, bot) <- Array.elems rooms]) ++ "#"
              , "  #" ++ replicate 8 '#'
              ]

instance Show Burrow2 where
    show (Burrow2 hallway rooms) = 
      unlines [ replicate 13 '#'
              , "#" ++ concatMap (maybe "." show) (Array.elems hallway) ++ "#"
              , "###" ++ concat (intersperse "#" [maybe "." show x | (x, _, _, _) <- Array.elems rooms]) ++ "###"
              , "  #" ++ concat (intersperse "#" [maybe "." show x | (_, x, _, _) <- Array.elems rooms]) ++ "#"
              , "  #" ++ concat (intersperse "#" [maybe "." show x | (_, _, x, _) <- Array.elems rooms]) ++ "#"
              , "  #" ++ concat (intersperse "#" [maybe "." show x | (_, _, _, x) <- Array.elems rooms]) ++ "#"
              , "  #" ++ replicate 8 '#'
              ]

done1 (Burrow1 hallway rooms) =
    and [ a == Just x && b == Just x | (x, (a, b)) <- Array.assocs rooms ]

-- Heuristic: number of amphipods not in the correct room
heur1 (Burrow1 hallway rooms) =
    sum [1 | (room, (a, b)) <- Array.assocs rooms, c <- [a, b], c /= Just room]

next1 burrow = moveIntoRooms1 burrow ++ moveIntoHallway1 burrow

-- Move amphipod in rooms out into the hallway
moveIntoHallway1 (Burrow1 hallway rooms) = do
    (room, (a, b)) <- Array.assocs rooms
    let h = start room -- Hallway index of room
    -- Find open positions in the hallway
    let left = takeWhile (vacant . snd) $ filter ((`notElem` [2, 4, 6, 8]) . snd) $ zip [1..] (reverse [0..h - 1])
    let right = takeWhile (vacant . snd) $ filter ((`notElem` [2, 4, 6, 8]) . snd) $ zip [1..] [h + 1..10]
    (numSteps, j) <- left ++ right

    case (a, b) of
        (Just x, _) -> do
          let stepCost = stepEnergy x * (numSteps + 1)
          return (Burrow1 (hallway // [(j, a)]) (rooms // [(room, (Nothing, b))]), stepCost)
        (Nothing, Just y) | y == room -> fail "locked in place"
        (Nothing, Just y) | y /= room -> do
          let stepCost = stepEnergy y * (numSteps + 2)
          return (Burrow1 (hallway // [(j, b)]) (rooms // [(room, (Nothing, Nothing))]), stepCost)
        (Nothing, Nothing) -> fail "room empty"
  where
    vacant i = isNothing (hallway ! i)

done2 (Burrow2 hallway rooms) =
    and [ all (== Just x) [a, b, c, d] | (x, (a, b, c, d)) <- Array.assocs rooms ]

next2 burrow = moveIntoRooms2 burrow ++ moveIntoHallway2 burrow

-- Move amphipods from hallway into rooms
moveIntoRooms2 (Burrow2 hallway rooms) = do
    (i, Just x) <- Array.assocs hallway

    -- Check that the hallway is clear 
    let steps = if i < start x then [i + 1..start x] else [i - 1, i - 2..start x]
        numSteps = length steps
    guard $ all (\j -> isNothing (hallway ! j)) steps
    
    let hallway' = hallway // [(i, Nothing)]

    -- Check that the amphipod's room is able to move into
    case rooms ! x of
        (Just _, _, _, _) -> fail "room is not vacant"
        (Nothing, Just y, Just z, Just w) | any (/= x) [y, z, w] ->
            fail "there's an alien amphipod in x's room"
        (Nothing, Just y, Just z, Just w) | all (== x) [y, z, w] -> do
            let stepCost = stepEnergy x * (numSteps + 1)
            return (Burrow2 hallway' (rooms // [(x, (Just x, Just y, Just z, Just w))]), stepCost)
        (Nothing, Nothing, Just z, Just w) | any (/= x) [z, w] ->
            fail "there's an alien amphipod in x's room"
        (Nothing, Nothing, Just z, Just w) | all (== x) [z, w] -> do
            let stepCost = stepEnergy x * (numSteps + 2)
            return (Burrow2 hallway' (rooms // [(x, (Nothing, Just x, Just z, Just w))]), stepCost)
        (Nothing, Nothing, Nothing, Just w) | any (/= x) [w] ->
            fail "there's an alien amphipod in x's room"
        (Nothing, Nothing, Nothing, Just w) | all (== x) [w] -> do
            let stepCost = stepEnergy x * (numSteps + 3)
            return (Burrow2 hallway' (rooms // [(x, (Nothing, Nothing, Just x, Just w))]), stepCost)
        (Nothing, Nothing, Nothing, Nothing) -> do
            let stepCost = stepEnergy x * (numSteps + 4)
            return (Burrow2 hallway' (rooms // [(x, (Nothing, Nothing, Nothing, Just x))]), stepCost)

-- Move amphipod in rooms out into the hallway
moveIntoHallway2 (Burrow2 hallway rooms) = do
    (room, (a, b, c, d)) <- Array.assocs rooms
    let h = start room -- Hallway index of room
    -- Find open positions in the hallway
    let left = takeWhile (vacant . snd) $ filter ((`notElem` [2, 4, 6, 8]) . snd) $ zip [1..] (reverse [0..h - 1])
    let right = takeWhile (vacant . snd) $ filter ((`notElem` [2, 4, 6, 8]) . snd) $ zip [1..] [h + 1..10]
    (numSteps, j) <- left ++ right

    case (a, b, c, d) of
        (Nothing, Nothing, Nothing, Nothing) -> fail "room empty"
        -- move the top-most amphipod
        (Just x, _, _, _) -> do
          let stepCost = stepEnergy x * (numSteps + 1)
          return (Burrow2 (hallway // [(j, a)]) (rooms // [(room, (Nothing, b, c, d))]), stepCost)
        (Nothing, Just x, _, _) -> do
          let stepCost = stepEnergy x * (numSteps + 2)
          return (Burrow2 (hallway // [(j, b)]) (rooms // [(room, (Nothing, Nothing, c, d))]), stepCost)
        (Nothing, Nothing, Just x, _) -> do
          let stepCost = stepEnergy x * (numSteps + 3)
          return (Burrow2 (hallway // [(j, c)]) (rooms // [(room, (Nothing, Nothing, Nothing, d))]), stepCost)
        (Nothing, Nothing, Nothing, Just x) -> do
          let stepCost = stepEnergy x * (numSteps + 4)
          return (Burrow2 (hallway // [(j, d)]) (rooms // [(room, (Nothing, Nothing, Nothing, Nothing))]), stepCost)
  where
    vacant i = isNothing (hallway ! i)

-- Move amphipods from hallway into rooms
moveIntoRooms1 (Burrow1 hallway rooms) = do
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
            return (Burrow1 hallway' (rooms // [(x, (Just x, Just y))]), stepCost)
        (Nothing, Just y) | y /= x ->
            fail "there's an alien amphipod in x's room"
        (Nothing, Nothing) -> do
            let stepCost = stepEnergy x * (numSteps + 2)
            return (Burrow1 hallway' (rooms // [(x, (Nothing, Just x))]), stepCost)

example1 = Burrow1 hallway rooms
  where
    hallway = Array.listArray (0, 10) (repeat Nothing)
    rooms = Array.array (A, D) [(A, (Just B, Just A)), (B, (Just C, Just D)), (C, (Just B, Just C)), (D, (Just D, Just A))]

example2 = Burrow2 hallway rooms
  where
    hallway = Array.listArray (0, 10) (repeat Nothing)
    rooms = Array.array (A, D) [(A, (Just B, Just D, Just D, Just A)), (B, (Just C, Just C, Just B, Just D)), (C, (Just B, Just B, Just A, Just C)), (D, (Just D, Just A, Just C, Just A))]

input1 = Burrow1 hallway rooms
  where
    hallway = Array.listArray (0, 10) (repeat Nothing)
    rooms = Array.array (A, D) [(A, (Just D, Just B)), (B, (Just B, Just D)), (C, (Just A, Just A)), (D, (Just C, Just C))]

input2 = Burrow2 hallway rooms
  where
    hallway = Array.listArray (0, 10) (repeat Nothing)
    rooms = Array.array (A, D) [(A, (Just D, Just D, Just D, Just B)), (B, (Just B, Just C, Just B, Just D)), (C, (Just A, Just B, Just A, Just A)), (D, (Just C, Just A, Just C, Just C))]

main = do
    -- Part 1
    case find (done1 . fst) (astar id next1 heur1 [input1]) of
        Just (_, ans1) -> print ans1

    -- Part 2
    case find (done2 . fst) (astar id next2 (const 0) [input2]) of
        Just (_, ans2) -> print ans2

-- 36434 too low
