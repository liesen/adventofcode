{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Data.List
import Data.Array
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Bits
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq (..))

data Problem = Problem
    { grid :: Array (Int, Int) Char
    , starts :: [(Int, Int)]
    , keyMap :: Map Char (Int, Int)
    }
  deriving (Show)

isWall, isKey, isDoor :: Char -> Bool
isWall x = x == '#'
isKey x = x >= 'a' && x <= 'z'
isDoor x = x >= 'A' && x <= 'Z'

parse :: String -> Problem
parse s =
    let rows = lines s
        maxrow = length rows - 1
        maxcol = length (head rows) - 1
        grid = listArray ((0, 0), (maxrow, maxcol)) (concat rows)
        starts = [ix | ix <- indices grid, grid ! ix == '@']
        keyMap = Map.fromList [(z, ix) | (ix, z) <- assocs grid, z >= 'a' && z <= 'z']
    in Problem{..}

-- Split a problem with many starting points into problems
-- with a single starting point
subproblems :: Problem -> [Problem]
subproblems problem@Problem{..} = map split starts
  where
    split start = 
        let xs = bfs id next start
            positions = map fst xs
            keyMap = Map.fromList [(z, pos) | pos <- positions, let z = grid ! pos, isKey z]
            starts = [start]
        in Problem{..}

    next (r, c) = do
        (dr, dc) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]
        let pos' = (r + dr, c + dc)
        let z = grid ! pos'
        guard $ not (isWall z)
        return (pos', 1)

-- Keys
-- Using a bit mask is slightly faster than an IntSet
-- type Keys = IntSet
newtype Keys = Keys Int deriving (Eq, Ord, Show)

instance Semigroup Keys where
    Keys a <> Keys b = Keys (a .|. b)

instance Monoid Keys where
    mempty = Keys 0

haveKey :: Keys -> Char -> Bool
-- haveKey mask x = IntSet.member (ord (toLower x) - ord 'a') mask
haveKey (Keys mask) x = testBit mask (ord (toLower x) - ord 'a')

addKey :: Keys -> Char -> Keys
-- addKey mask x = IntSet.insert (ord x - ord 'a') mask
addKey (Keys mask) x = Keys $ setBit mask (ord x - ord 'a')

-- BFS state
data State = State { problem :: Problem, position :: (Int, Int), keys :: Keys }

bfs :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> a -> [(a, Int)]
bfs rep next start = go mempty (Sequence.singleton (start, 0))
  where
    go seen Empty = []
    go seen ((x, cost) :<| q1) 
      | Set.member r seen = go seen q1
      | otherwise = (x, cost) : go seen1 q2
      where
        r = rep x
        seen1 = Set.insert r seen
        q2 = q1 <> Sequence.fromList (map (\(x', stepcost) -> (x', cost + stepcost)) (next x))

-- State transition
next :: Bool -> State -> [(State, Int)]
next intangibility s@State{..} = do
    (dr, dc) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]
    let pos' = (r + dr, c + dc)
    let z = (grid problem) ! pos'

    -- Stop at walls (also does bounds checking)
    guard $ not (isWall z)

    -- Don't go through locked doors unless we have intangibility
    guard $ not (isDoor z) || haveKey keys z || intangibility

    if isKey z
        then return (s {position = pos', keys = addKey keys z}, 1)
        else return (s {position = pos'}, 1)
  where
    (r, c) = position

-- State representation
rep State{..} = (position, keys)

done State{..} = keys == allKeys
  where
    -- allKeys = IntSet.fromList $ map (\c -> ord c - ord 'a') $ Map.keys (keyMap problem)
    allKeys = Keys $ foldl' setBit zeroBits $ map (\c -> ord c - ord 'a') $ Map.keys (keyMap problem)

main = do
    -- Part 1
    input <- readFile "input.txt"
    let problem@Problem{..} = parse input
        [position] = starts
        keys = mempty
        Just (x, ans) = find (done . fst) $ bfs rep (next False) State{..}
    print ans
    
    -- Part 2
    -- Sum the shortest paths to collect all keys, moving through locked doors
    -- if needed, for each starting position. I am not sure why this works, but
    -- it also worked here: https://github.com/marcodelmastro/AdventOfCode2019/blob/master/Day%2018.ipynb
    input <- readFile "input2.txt"
    let problem@Problem{..} = parse input
    subsolutions <- forM (subproblems problem) $ \problem@Problem{..} -> do
        let [position] = starts
            keys = mempty
            Just (x, ans) = find (done . fst) $ bfs rep (next True) State{..}
        return ans
    print (sum subsolutions)