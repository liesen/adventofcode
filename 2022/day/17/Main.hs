{-# LANGUAGE ImportQualifiedPost #-}
import Control.Monad
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Foldable
import Control.Arrow
import Data.Maybe

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
    ymin' = minimum (Set.map fst s')
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
    ymin = minimum (Set.map fst s)

data State = State Rocks (Maybe RockId) String [Shape]

run program = iterate f (State chamber Nothing (cycle program) (cycle shapes))
  where
    chamber = Set.fromList [(0, x) | x <- [0..6]] -- Add artificial floor
    f (State rocks _ p (s:ss)) = State (rocks <> s') (Just x) p' ss
      where
        (Shape x s', p') = move rocks (adjust rocks s) p

main = do
    input <- readFile "input.txt"
    let program = head (lines input)

    -- Part 2
    let State rocks1 _ _ _ = run program !! 2022
        ans1 = maximum (Set.map fst rocks1)
    print ans1

