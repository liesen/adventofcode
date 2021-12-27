{-# LANGUAGE ImportQualifiedPost #-}
import Data.Either
import Data.Set (Set)
import Data.Set qualified as Set

type Herd = Set (Int, Int)

data Seafloor = Seafloor (Int, Int) Herd Herd deriving (Eq)

pp (Seafloor (numrows, numcols) east south) =
    unlines [[coordToChar (r, c) | c <- [0..numcols - 1]] | r <- [0..numrows - 1]]
  where
    coordToChar p
        | p `Set.member` east = '>'
        | p `Set.member` south = 'v'
        | otherwise = '.'

parse input = Seafloor bounds (Set.fromList east) (Set.fromList south)
  where
    numrows = length (lines input)
    numcols = length (head (lines input))
    bounds = (numrows, numcols)

    (east, south) = partitionEithers $
        [ if x == '>' then Left (r, c) else Right (r, c)
        | (r, cs) <- zip [0..] (lines input)
        , (c, x) <- zip [0..] cs
        , x `elem` "v>"
        ]

step (Seafloor bounds@(numrows, numcols) east south) = Seafloor bounds east' south'
  where
    east' = Set.map (move (east <> south) (0, 1)) east
    south' = Set.map (move (east' <> south) (1, 0)) south

    move cucumbers (dr, dc) p@(r, c)
        | Set.member p' cucumbers = p
        | otherwise = p'
        where p' = ((r + dr) `mod` numrows, (c + dc) `mod` numcols)

main = do
    input <- readFile "input.txt"
    let seafloor = parse input
        steps = iterate step seafloor

    -- Part 1
    print $ length (takeWhile (uncurry (/=)) (zip steps (tail steps))) + 1

