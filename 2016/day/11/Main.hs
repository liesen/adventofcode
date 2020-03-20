{-# LANGUAGE RecordWildCards, ViewPatterns #-}
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Monoid
import Data.Function (on)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

data Item =
      Microchip String
    | RTG String
  deriving (Show, Eq, Ord)

data Elevator = Elevator { _elevator :: Int, _steps :: Int, _items :: [(Int, Item)] } deriving (Show)

floor "first" = 1
floor "second" = 2
floor "third" = 3
floor "fourth" = 4
floor "fifth" = 5

parse = Elevator 1 0 . concatMap parseLine . lines

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
step s =
    -- Move any one item
    [ s'
    | item@(f, _) <- _items s
    , f == _elevator s
    , s' <- [moves succ [item], moves pred [item]]
    , validState s'
    ]
    ++
    -- Move any two items
    [ s'
    | item1@(f1, x1) <- _items s, f1 == _elevator s
    , item2@(f2, x2) <- _items s, f2 == _elevator s
    , item1 /= item2
    , s' <- [moves succ [item1, item2], moves pred [item1, item2]]
    , validState s'
    ]
  where
    -- Move items up or down
    moves f items =
        s { _elevator = f (_elevator s),
            _steps = _steps s + 1,
            _items = foldr insert (foldr delete (_items s) items) (map (\(floor, item) -> (f floor, item)) items)
          }

testInput = [
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.",
    "The second floor contains a hydrogen generator.",
    "The third floor contains a lithium generator.",
    "The fourth floor contains nothing relevant."
  ]

testElevator = parse (unlines testInput)

bfs :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfs rep next start = loop Set.empty (Seq.fromList [start])
    where
        loop _    Empty = []
        loop seen (x :<| q1)
            | Set.member r seen = loop seen q1
            | otherwise = x : loop seen1 q2
            where
                r = rep x
                seen1 = Set.insert r seen
                q2 = q1 <> Seq.fromList (next x)

isSolution s = _elevator s == 4 && all ((== 4) . fst) (_items s)

solve = filter isSolution . bfs rep step
  where
    rep Elevator{..} = (_elevator, _items)  -- Exclude number of steps in the state representation

main = do
    input <- readFile "input.txt"
    let facility = parse input
    print $ solve facility