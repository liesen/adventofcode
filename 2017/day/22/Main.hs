module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

data State = State (Int, Int) (Int, Int) (Set (Int, Int)) Int

infectionCount (State _ _ _ x) = x

turnRight, turnLeft :: (Int, Int) -> (Int, Int)
turnRight (1, 0)  = (0, 1)
turnRight (0, 1)  = (-1, 0)
turnRight (-1, 0) = (0, -1)
turnRight (0, -1) = (1, 0)

turnLeft (1, 0)  = (0, -1)
turnLeft (0, -1) = (-1, 0)
turnLeft (-1, 0) = (0, 1)
turnLeft (0, 1)  = (1, 0)

test = State (0, 0) (0, -1) (Set.fromList [(-1, 0), (1, -1)]) 0

step :: State -> State
step (State pos@(x,y) dir infected count)
    | Set.member pos infected =
        let (dx, dy) = turnRight dir
        in State (x + dx, y + dy) (dx, dy) (Set.delete pos infected) count
    | otherwise               =
        let (dx, dy) = turnLeft dir
        in State (x + dx, y + dy) (dx, dy) (Set.insert pos infected) (count + 1)

instance Show State where
    show (State pos dir infected count) = unlines [concat [f (x, y)| x <- [minX - 2..maxX + 2]] | y <- [minY - 2..maxY + 2]]
      where
        minX = minimum $ map fst $ Set.toList infected
        maxX = maximum $ map fst $ Set.toList infected
        minY = minimum $ map snd $ Set.toList infected
        maxY = maximum $ map snd $ Set.toList infected
        f (x, y) = let z = if Set.member (x, y) infected then '#' else '.'
                   in if (x, y) == pos
                        then '[':z:"]"
                        else ' ':z:" "

-- parse :: String -> State
parse s = State (maxX `div` 2, maxY `div` 2) (0, -1) infected 0
  where
    ys = lines s
    maxY = length ys
    maxX = maximum (map length ys) 
    infected = Set.fromList [(x, y) | (y, xs) <- zip [0..] ys, (x, z) <- zip [0..] xs, z == '#']

data Flag = Clean | Weakened | Infected | Flagged deriving Enum

instance Show Flag where
    show Clean = "."
    show Weakened = "W"
    show Infected = "#"
    show Flagged = "F"

data State2 = State2 (Int, Int) (Int, Int) (Map (Int, Int) Flag) Int

instance Show State2 where
    show (State2 pos dir nodes count) = unlines [concat [f (x, y)| x <- [minX - 2..maxX + 2]] | y <- [minY - 2..maxY + 2]]
      where
        minX = minimum $ map fst $ Map.keys nodes
        maxX = maximum $ map fst $ Map.keys nodes
        minY = minimum $ map snd $ Map.keys nodes
        maxY = maximum $ map snd $ Map.keys nodes
        f (x, y) = let z = case Map.lookup (x, y) nodes of
                             Nothing -> '.'
                             Just a  -> head (show a)
                   in if (x, y) == pos
                        then '[':z:"]"
                        else ' ':z:" "

step2 :: State2 -> State2
step2 (State2 pos@(x,y) dir@(dx, dy) nodes count) =
    case Map.lookup pos nodes of
      Nothing       -> let (dx, dy) = turnLeft dir in State2 (x + dx, y + dy) (dx, dy) (Map.insert pos Weakened nodes) count
      Just Weakened -> State2 (x + dx, y + dy) dir (Map.adjust (const Infected) pos nodes) (count + 1)
      Just Infected -> let (dx, dy) = turnRight dir in State2 (x + dx, y + dy) (dx, dy) (Map.adjust (const Flagged) pos nodes) count
      Just Flagged -> let (dx, dy) = turnLeft (turnLeft dir) in State2 (x + dx, y + dy) (dx, dy) (Map.delete pos nodes) count
  
parse2 s = let (State pos dir infected count) = parse s
           in State2 pos dir (Map.fromList (zip (Set.toList infected) (repeat Infected))) count

infectionCount2 (State2 _ _ _ x) = x

test2 = State2 (0, 0) (0, -1) (Map.fromList [((-1, 0), Infected), ((1, -1), Infected)]) 0

main = do
    input <- readFile "input.txt"
    
    -- Part 1
    let state1 = parse input
    print $ infectionCount $ iterate step state1 !! 10000

    -- Part 2
    let state2 = parse2 input
    print $ infectionCount2 $ iterate step2 state2 !! 10000000
