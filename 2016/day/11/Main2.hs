{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Function (on)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

floors = ["first", "second", "third", "fourth"] `zip` [1..4]

-- floor (flip findIndex floors . (==) -> Just n) = succ n
floor (flip lookup floors -> Just n) = n

data Item =
      Microchip String
    | RTG String
  deriving (Show, Eq, Ord)

data Elevator = Elevator { _elevator :: Int, _items :: [(Int, Item)] } deriving (Show, Eq, Ord)

elevator = Elevator 1 . sort . concatMap parse

testInput = [
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.",
    "The second floor contains a hydrogen generator.",
    "The third floor contains a lithium generator.",
    "The fourth floor contains nothing relevant."
  ]

testElevator = elevator testInput

valid s =
    0 < _elevator s && _elevator s < 5 && and [validFloor xs | (map snd -> xs) <- groupBy ((==) `on` fst) (_items s)]
  where
    validFloor xs = and [{- No RTGs -} null z || {- It's own RTG is on the same floor -} or z | Microchip x <- xs, let z = [x == y | RTG y <- xs]]

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
                 guard $ valid s'
                 guard $ s' `Set.notMember` seen
                 return (s', m + 1)
        
        move s f items =
              s { _items = foldr insert (foldr delete (_items s) items) (map (\(floor, item) -> (f floor, item)) items),
                  _elevator = f (_elevator s)
                }

        heuristic = id -- sortOn (negate . sum . map fst . _items . fst)

main = readFile "input.txt" >>= print . search1 . elevator . lines
-- main = print $ search1 testElevator
