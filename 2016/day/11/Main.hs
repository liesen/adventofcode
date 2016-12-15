{-# LANGUAGE ViewPatterns, RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Ord

import Data.Set (Set)
import qualified Data.Set as Set

--- Day 11: Radioisotope Thermoelectric RTGs ---

-- The description sound's like a generalized version of the riddle
-- of crossing the river with a wolf, a goat & a cabbage:
-- 
--   https://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle
--
--   https://en.wikipedia.org/wiki/River_crossing_puzzle

floors = ["first", "second", "third", "fourth", "fifth"]

floor = succ . fromJust . flip findIndex floors . (==)

data Item =
      Microchip String
    | RTG String
  deriving (Show, Eq, Ord)

data Elevator = Elevator { _elevator :: Int, _items :: [(Int, Item)] } deriving (Show, Eq, Ord)

data State = State Elevator (Set Elevator) deriving Show

parse (words -> ("The":nth:"floor":"contains":xs)) = map (\item -> (Main.floor nth, item)) (items xs)

items [] = []
items ["nothing", "relevant."] = []
items ("and":xs) = items xs
items ("a":stuff:"microchip":xs) = let (stuff', "-compatible") = break (== '-') stuff in Microchip stuff' : items xs
items ("a":stuff:"microchip,":xs) = let (stuff', "-compatible") = break (== '-') stuff in Microchip stuff' : items xs
items ("a":stuff:"microchip.":[]) = let (stuff', "-compatible") = break (== '-') stuff in [Microchip stuff']
items ("a":stuff:"generator":xs) = RTG stuff : items xs
items ("a":stuff:"generator,":xs) = RTG stuff : items xs
items ("a":stuff:"generator.":[]) = [RTG stuff]

testInvalidState = Elevator 1 [(1, Microchip "x"), (1, RTG "y")]
testValidState = Elevator 1 [(1, Microchip "x"), (1, RTG "y"), (1, RTG "x")]
testValidState2 = Elevator 1 [(1, Microchip "x"), (1, Microchip "y")] -- Microchips can 

validState Elevator{..} =
    0 < _elevator && _elevator < 5 && and [validFloor xs | (map snd -> xs) <- groupBy ((==) `on` fst) _items]
  where
    validFloor xs = and [{- No RTGs -} null z || {- It's own RTG is on the same floor -} or z | Microchip x <- xs, let z = [x == y | RTG y <- xs]]

testInput = [
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.",
    "The second floor contains a hydrogen generator.",
    "The third floor contains a lithium generator.",
    "The fourth floor contains nothing relevant."
  ]

testElevator = Elevator 1 . sort . concatMap parse $ testInput

data Move = Move { _dir :: Int, _cargo :: [Item] } deriving (Show)

type Space m s = [(m, s)]

type Strategy m s = Space m s -> Space m s -> Space m s

class Monoid m => SearchProblem s m where
    trans :: s -> [(m, s)]
    isSolution :: (m, s) -> Bool
    space, solutions :: Strategy m s -> s -> Space m s

    space f s = expand f (step (mempty, s))
      where
        expand f []     = []
        expand f (s:ss) = s : expand f (f (step s) ss)
        step (ms, s)    = [(ms `mappend` m, t) | (m, t) <- trans s]

    solutions f = filter isSolution . space f

instance SearchProblem State (Sum Int) where
    trans (State s seen) = ss
      where
        moves f items =
          let m = Sum 1
              s' = s { _items = foldr insert (foldr delete (_items s) items) (map (\(floor, item) -> (f floor, item)) items),
                       _elevator = f (_elevator s)
                     }
          in (m, s')

        ss :: [(Sum Int, State)]
        ss = -- Move any one item
             [ (m, State s' seen')
             | item@(f, _) <- _items s,
               f == _elevator s,
               (m, s') <- [moves succ [item], moves pred [item]],
               validState s',
               s' `Set.notMember` seen]
             ++
             -- Move any two items
             [ (m, State s' seen')
             | item1@(f1, x1) <- _items s, f1 == _elevator s,
               item2@(f2, x2) <- _items s, f2 == _elevator s,
               item1 /= item2,
               (m, s') <- [moves succ [item1, item2], moves pred [item1, item2]],
               validState s',
               s' `Set.notMember` seen]
        seen' = seen `Set.union` Set.fromList (map (\(_, (State s _)) -> s) ss)

    isSolution (_, State s _seen) = _elevator s == 4 && all ((== 4) . fst) (_items s)

dfs = (++)
bfs = flip dfs

heuristic :: (Num m, Ord m) => Strategy m State -> Strategy m State
heuristic f xs ys = sortBy (comparing cmp) $ f xs ys
  where cmp (m, (State s seen)) = (negate . sum . map fst . _items $ s, negate m)

solution :: Elevator -> Space (Sum Int) State
solution s = solutions (heuristic bfs) (State s (Set.singleton s))

input = readFile "input.txt" >>= return . Elevator 1 . sort . concatMap parse . lines

test = head . solution $ testElevator

-- main = input >>= print . head . solution

search1 :: Elevator -> (Elevator, Int)
search1 elevator = search' [(elevator, 0)] Set.empty
  where
    isSol s = all ((== 4) . fst) (_items s)
    search' :: [(Elevator, Int)] -> Set Elevator -> (Elevator, Int)
    search' ps seen
        | (p:_) <- filter (isSol . fst) ps = p
        | otherwise                        = search' (heuristic ps') seen'
      where
        seen' = seen `Set.union` Set.fromList (map fst ps')
        ps' = do (s, m) <- ps
                 let items = [item | item@(f, _) <- _items s, f == _elevator s]
                 s' <- concat [
                     -- Move any one item
                     [t | item <- items, t <- [move s succ [item], move s pred [item]]],
                     -- Move any two items
                     [t | item1 <- items, item2 <- items, item1 /= item2, t <- [move s succ [item1, item2], move s pred [item1, item2]]]
                   ]
                 guard $ validState s'
                 guard $ s' `Set.notMember` seen
                 return (s', m + 1)
        
        move s f items =
              s { _items = foldr insert (foldr delete (_items s) items) (map (\(floor, item) -> (f floor, item)) items),
                  _elevator = f (_elevator s)
                }

        heuristic = sortOn (negate . sum . map fst . _items . fst)

-- main = input >>= print . search1
main = print $ search1 testElevator 
