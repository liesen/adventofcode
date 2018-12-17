{-# LANGUAGE RecordWildCards #-}
import Data.Function (on)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

turnLeft '<' = 'v'
turnLeft 'v' = '>'
turnLeft '>' = '^'
turnLeft '^' = '<'

turnRight '<' = '^'
turnRight 'v' = '<'
turnRight '>' = 'v'
turnRight '^' = '>'

straight = id


-- All coordinates are (y, x) because it's more convenient for
-- indexing into the tracks (lines by rows) and sorting: "carts
-- on the top row move first (acting left to right)".

data Cart = Cart
    { cart :: Char
    , numTurns :: Int
    } deriving (Show, Eq)

data Tracks = Tracks
    { tracks :: [[Char]]
    , carts :: [((Int, Int), Cart)]  -- Can have collisions
    } deriving (Show)

showTracks Tracks{..} = unlines [ [f (i, j) x | (j, x) <- zip [0..] xs]
                                | (i, xs) <- zip [0..] tracks
                                ]
  where
    f ij x = maybe x cart (lookup ij carts)

parse input = Tracks xss' carts'
  where
    xss = lines input
    xss' = [[if x `elem` "<>" then '-' else if x `elem` "^v" then '|' else x | x <- xs] | xs <- xss]
    carts' = [((i, j), Cart x 0) | (i, xs) <- zip [0..] xss, (j, x) <- zip [0..] xs, x `elem` "<v>^"]

tick t = t { carts = map (updateCart (tracks t)) (sortBy (comparing fst) (carts t)) }

hasCollisions = not . null . collisions

collisions = filter ((> 1) . length) . groupBy ((==) `on` fst) . sortBy (comparing fst) . carts

-- This does not work as expected because collisions have to be resolved
-- also with carts that (during a tick) has not yet moved
removeCollisions t = t { carts = carts' }
  where
    carts' = concat $ filter ((== 1) . length) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ carts t

tickRemoveCollisions :: Tracks -> Tracks
tickRemoveCollisions t = t { carts = carts' }
  where
    carts' = go (sortBy (comparing fst) (carts t)) []
    go []     cs' = cs'
    go (c:cs) cs' =
        let c' = updateCart (tracks t) c
        in case (
            -- Collisions with carts that hasn't yet moved
            filter ((== fst c') . fst) cs,
            -- Collisions with carts that have moved
            filter ((== fst c') . fst) cs') of
               ([], []) -> go cs (c':cs')
               (xs, xs') -> go (cs \\ xs) (cs' \\ xs')

updateCart xss ((i, j), (Cart z t)) = 
    let (i', j') = case z of
            '<' -> (  i  , j - 1)
            'v' -> (i + 1,   j  )
            '>' -> (  i  , j + 1)
            '^' -> (i - 1,   j  )
    in case xss !! i' !! j' of
        '+' -> ((i', j'), Cart (([turnLeft, straight, turnRight] !! t) z) ((t + 1) `mod` 3))
        '/' -> case z of
                   '<' -> ((i', j'), Cart 'v' t)
                   'v' -> ((i', j'), Cart '<' t)
                   '>' -> ((i', j'), Cart '^' t)
                   '^' -> ((i', j'), Cart '>' t)
        '\\' -> case z of
                    '<' -> ((i', j'), Cart '^' t)
                    'v' -> ((i', j'), Cart '>' t)
                    '>' -> ((i', j'), Cart 'v' t)
                    '^' -> ((i', j'), Cart '<' t)
        _ -> ((i', j'), Cart z t)

swap (i, j) = (j, i)


main = do
    input <- readFile "input.txt"
    let a = parse input

    -- Part 1
    print $ swap . fst . head . head $ collisions $ until hasCollisions tick a

    -- Part 2
    -- DENIED on: print $ carts $ until ((== 1) . length . carts) (removeCollisions . tick) a
    print $ swap . fst . head $ carts $ until ((== 1) . length . carts) tickRemoveCollisions a
