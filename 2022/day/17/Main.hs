{-# LANGUAGE ImportQualifiedPost #-}
import Control.Monad
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Foldable
import Control.Arrow
import Data.Maybe


type Shape = Set (Int, Int)

hbar, plus, vbar, edge, square :: Shape
hbar = Set.fromList [(0, x) | x <- [0..3]]
plus = Set.fromList [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
vbar = Set.fromList [(y, 0) | y <- [0..3]]
edge = Set.fromList [(0, 2), (1, 2), (2, 2), (0, 1), (0, 0)]
square = Set.fromList [(x, y) | y <- [0, 1], x <- [0, 1]]

shapes = [hbar, plus, edge, vbar, square]

moveLeft, moveRight :: Shape -> Shape -> Shape
moveLeft rocks shape = hmove rocks shape (-1)
moveRight rocks shape = hmove rocks shape 1

hmove rocks shape dx
    | not (Set.disjoint rocks shape') = shape  -- ??? move horizontally into rocks
    | xmin < 0 || xmax > 6 = shape
    | otherwise = shape'
  where shape' = Set.map (second (+ dx)) shape
        xmin = minimum (Set.map snd shape')
        xmax = maximum (Set.map snd shape')

moveDown rocks shape
    | ymin' == 0 = fail "hit the floor"
    | not (Set.disjoint shape' rocks) = fail "hit resting rock"
    | otherwise = pure shape'
  where
    shape' = Set.map (first (+ (-1))) shape
    ymin' = minimum (Set.map fst shape')

move rocks shape (x:xs) =
    case moveDown rocks shape' of
        Nothing -> (shape', xs)
        Just shape'' -> move rocks shape'' xs
  where
    hmove | x == '<' = moveLeft
          | x == '>' = moveRight
    shape' = hmove rocks shape

-- Adjust starting position
adjust rocks shape = Set.map (((+ yfloor) . (+ ymin) . (+ 3) . (+ 1)) *** (+ 2)) shape
  where
    yfloor = fromMaybe 0 (Set.lookupMax (Set.map fst rocks))
    ymin = Set.findMin (Set.map fst shape)

run program = go mempty (cycle program) (cycle shapes)

go rocks program []     = []
go rocks program (s:ss) =
    let (s', program') = move rocks (adjust rocks s) program
        rocks' = rocks <> s'
    in rocks : go rocks' program' ss

main = do
    input <- readFile "input.txt"
    let program = head (lines input)

    -- Part 2
    let tower1 = run program !! 2022
        ans1 = maximum (Set.map fst tower1)
    print ans1

