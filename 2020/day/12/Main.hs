{-# LANGUAGE ViewPatterns #-}

data Ship = Ship Int Int Int deriving (Eq, Ord, Show)

-- Ship y x a
--  +y is north, -y is south
--  +x is east, -x is west
--  assume a is always in {0,90,180,270}
step :: Ship -> String -> Ship
step (Ship y x a) ('N':(read -> n)) = Ship (y + n) x a
step (Ship y x a) ('S':(read -> n)) = Ship (y - n) x a
step (Ship y x a) ('E':(read -> n)) = Ship y (x + n) a
step (Ship y x a) ('W':(read -> n)) = Ship y (x - n) a
step (Ship y x a) ('L':(read -> n)) = Ship y x ((a + n) `mod` 360)
step (Ship y x a) ('R':(read -> n)) = Ship y x ((a - n) `mod` 360)
step (Ship y x a) ('F':(read -> n)) =
    case a of
        0   -> Ship y (x + n) a
        90  -> Ship (y + n) x a
        180 -> Ship y (x - n) a
        270 -> Ship (y - n) x a

distance (Ship y x a) = abs y + abs x

data Waypoint =  Waypoint Int Int deriving (Eq, Ord, Show)

stepwp :: (Ship, Waypoint) -> String -> (Ship, Waypoint)
stepwp (s, w@(Waypoint wy wx)) ('N':(read -> n)) = (s, Waypoint (wy + n) wx)
stepwp (s, w@(Waypoint wy wx)) ('S':(read -> n)) = (s, Waypoint (wy - n) wx)
stepwp (s, w@(Waypoint wy wx)) ('E':(read -> n)) = (s, Waypoint wy (wx + n))
stepwp (s, w@(Waypoint wy wx)) ('W':(read -> n)) = (s, Waypoint wy (wx - n))
stepwp (s, w@(Waypoint wy wx)) "L90"  = (s, Waypoint wx (-wy))
stepwp (s, w@(Waypoint wy wx)) "L180" = (s, Waypoint (-wy) (-wx))
stepwp (s, w@(Waypoint wy wx)) "L270" = (s, Waypoint (-wx) wy)
stepwp (s, w@(Waypoint wy wx)) "R90"  = (s, Waypoint (-wx) wy)
stepwp (s, w@(Waypoint wy wx)) "R180" = (s, Waypoint (-wy) (-wx))
stepwp (s, w@(Waypoint wy wx)) "R270" = (s, Waypoint wx (-wy))
stepwp ((Ship y x a), w@(Waypoint wy wx)) ('F':(read -> n)) = ((Ship (y + wy * n) (x + wx * n) a), w)

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ distance $ foldl step (Ship 0 0 0) (lines input)
    
    -- Part 2
    print $ distance . fst $ foldl stepwp ((Ship 0 0 0), (Waypoint 1 10)) (lines input)