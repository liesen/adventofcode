{-# LANGUAGE NumericUnderscores, ImportQualifiedPost #-}
import Control.Arrow
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set


example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

type Rocks = Set (Int, Int)

data RockId = HBar | Plus | Edge | VBar | Square
  deriving (Eq, Ord, Show)

data Shape = Shape RockId Rocks

hbar, plus, vbar, edge, square :: Shape
hbar = Shape HBar $ Set.fromList [(0, x) | x <- [0..3]]
plus = Shape Plus $ Set.fromList [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
edge = Shape Edge $ Set.fromList [(0, 2), (1, 2), (2, 2), (0, 1), (0, 0)]
vbar = Shape VBar $ Set.fromList [(y, 0) | y <- [0..3]]
square = Shape Square $ Set.fromList [(x, y) | y <- [0, 1], x <- [0, 1]]

shapes = [
        hbar,
        plus,
        edge,
        vbar,
        square
    ]

moveLeft, moveRight :: Rocks -> Shape -> Shape
moveLeft rocks shape = hmove rocks shape (-1)
moveRight rocks shape = hmove rocks shape 1

hmove rocks shape@(Shape name s) dx
    | not (Set.disjoint rocks s') = shape  -- ??? move horizontally into rocks
    | xmin < 0 || xmax > 6 = shape
    | otherwise = Shape name s'
  where s' = Set.map (second (+ dx)) s
        xmin = minimum (Set.map snd s')
        xmax = maximum (Set.map snd s')

moveDown rocks shape@(Shape name s)
    | not (Set.disjoint s' rocks) = fail "hit floor or resting rock"
    | otherwise = pure shape'
  where
    s' = Set.map (first (+ (-1))) s
    shape' = Shape name s'

move rocks shape@(Shape name s) (x:xs) =
    case moveDown rocks shape' of
        Nothing -> (shape', xs)
        Just shape'' -> move rocks shape'' xs
  where
    hmove | x == '<' = moveLeft
          | x == '>' = moveRight
    shape' = hmove rocks shape

-- Adjust starting position
adjust rocks shape@(Shape name s) =
    Shape name $ Set.map (((+ yfloor) . (+ ymin) . (+ 3) . (+ 1)) *** (+ 2)) s
  where
    yfloor = maximum (Set.map fst rocks)
    ymin = 0 -- minimum y for all shapes is 0

data State = State
    Rocks  -- Resting rocks
    (Maybe RockId)  -- Last placed "piece"
    String  -- Program (repeating!)
    [Shape]  -- Shapes (repeating!)

height (State rocks _ _ _) = maximum (Set.map fst rocks)

-- Keep dropping rocks...
run program = iterate f (State chamber Nothing (cycle program) (cycle shapes))
  where
    chamber = Set.fromList [(0, x) | x <- [0..6]] -- Add artificial floor
    f (State rocks _ p (s:ss)) = State (rocks <> s') (Just x) p' ss
      where
        (Shape x s', p') = move rocks (adjust rocks s) p

-- Part 2: Keep track of the last piece dropped and the upper 20 (10 didn't
-- work) resting rocks
megarun = go mempty 0 0 . run

rep (State rocks (Just x) _ _) = (x, crust)
  where
    ymax = maximum (Set.map fst rocks)
    crust = Set.map (first (ymax -)) $ Set.filter (\(y, x) -> ymax - y <= 20) rocks

go seen t yskip [] = error "impossible"
go seen t yskip ((State _ Nothing _ _):ss) = go seen (t + 1) yskip ss
go seen t yskip (s:ss)
    | t > bigT = error "overshot"
    | t == bigT = ymax + yskip
    | Just (t_, ymax_) <- Map.lookup r seen =
        -- Same rep was found at t_: we have found a cycle!
        let dt = t - t_
            dy = ymax - ymax_
            n = (bigT - t) `div` dt
            t' = t + n * dt
        in if t' <= bigT
            then go (Map.insert r (t', ymax) seen) (t' + 1) (yskip + n * dy) ss
            else go seen' (t + 1) yskip ss
    | otherwise = go seen' (t + 1) yskip ss
  where
    ymax = height s
    r = rep s
    seen' = Map.insert r (t, ymax) seen
    bigT = 1_000_000_000_000

main = do
    input <- readFile "input.txt"
    let program = head (lines input)

    -- Part 1
    print $ height $ run program !! 2022

    -- Part 2
    print $ megarun program
