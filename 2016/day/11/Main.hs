{-# LANGUAGE RecordWildCards, ViewPatterns #-}
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Monoid
import Data.Function (on)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

data Item
    = Microchip String
    | RTG String
    deriving (Show, Eq, Ord)

data Elevator = Elevator
    { _elevator :: Int
    , _items :: [(Int, Item)]  -- It's important that _elems is sorted
    } deriving (Eq, Ord, Show)

floor "first" = 1
floor "second" = 2
floor "third" = 3
floor "fourth" = 4
floor "fifth" = 5

parse = Elevator 1 . sort . concatMap parseLine . lines

parseLine (words -> ("The":nth:"floor":"contains":xs)) = map (\item -> (Main.floor nth, item)) (items xs)

items [] = []
items ["nothing", "relevant."] = []
items ("and":xs) = items xs
items ("a":stuff:"microchip":xs) = let (stuff', "-compatible") = break (== '-') stuff in Microchip stuff' : items xs
items ("a":stuff:"microchip,":xs) = let (stuff', "-compatible") = break (== '-') stuff in Microchip stuff' : items xs
items ("a":stuff:"microchip.":[]) = let (stuff', "-compatible") = break (== '-') stuff in [Microchip stuff']
items ("a":stuff:"generator":xs) = RTG stuff : items xs
items ("a":stuff:"generator,":xs) = RTG stuff : items xs
items ("a":stuff:"generator.":[]) = [RTG stuff]

validState Elevator{..} =
    0 < _elevator && _elevator < 5 && and [validFloor xs | (map snd -> xs) <- groupBy ((==) `on` fst) _items]
  where
    validFloor xs = and [{- No RTGs -} null z || {- It's own RTG is on the same floor -} or z | Microchip x <- xs, let z = [x == y | RTG y <- xs]]

-- Generate set of possible states given the current state
step :: Elevator -> [(Elevator, Int)]
step s =
    -- Move any one item
    [ (s', 1)
    | item@(f, _) <- _items s
    , f == _elevator s
    , s' <- [moves succ [item], moves pred [item]]
    , validState s'
    ]
    ++
    -- Move any two items
    [ (s', 1)
    | item1@(f1, x1) <- _items s, f1 == _elevator s
    , item2@(f2, x2) <- _items s, f2 == _elevator s
    , item1 /= item2
    , s' <- [moves succ [item1, item2], moves pred [item1, item2]]
    , validState s'
    ]
  where
    -- Move items up or down
    moves f items =
        s { _elevator = f (_elevator s)
          , _items = foldr insert (foldr delete (_items s) items) (map (\(floor, item) -> (f floor, item)) items)
          }

bfs :: Ord r => (a -> r) -> (a -> [(a, Int)]) -> a -> [(a, Int)]
bfs rep next start = loop Set.empty (Seq.fromList [(start, 0)])
  where
    loop _    Empty = []
    loop seen ((x, cost) :<| q1)
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = q1 <> Seq.fromList (map (\(x', stepcost) -> (x', cost + stepcost)) (next x))

isSolution Elevator{..} = _elevator == 4 && all ((== 4) . fst) _items

main = do
    input <- readFile "input.txt"
    let facility1 = parse input

    -- Part 1
    print $ snd . head . filter (isSolution . fst) $ bfs id step facility1

    -- Part 2
    let facility2 = parse $ "The first floor contains a elerium generator, a elerium-compatible microchip, a dilithium generator and a dilithium-compatible microchip.\n" ++ input
    print $ snd . head . filter (isSolution . fst) $ bfs id step facility2
