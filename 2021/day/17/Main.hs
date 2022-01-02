import Control.Monad
import Data.Ix (inRange)
import Data.List
import Data.Maybe

data Probe = Probe 
    { vel :: (Int, Int)
    , pos :: (Int, Int)
    }
    deriving (Show)

type TargetArea = ((Int, Int), (Int, Int))

step (Probe (dx, dy) (x, y))
    = Probe (dx - signum dx, dy - 1) (x + dx, y + dy)

shoot targetArea = takeWhile (hasPotential targetArea) . iterate step

hasPotential ((xmin, ymin), (xmax, ymax)) (Probe (dx, dy) (x, y)) = y >= ymin

pp targetArea@((tx0, ty0), (tx1, ty1)) trajectory = 
    unlines $ reverse [
        [char (x, y) | x <- [xmin..xmax]]
        | y <- [ymin..ymax]
    ]
    where
        poss = map pos trajectory
        xmin = minimum (tx0 : tx1 : map fst poss)
        xmax = maximum (tx0 : tx1 : map fst poss)
        ymin = minimum (ty0 : ty1 : map snd poss)
        ymax = maximum (ty0 : ty1 : map snd poss)
        char p
            | p == (0, 0) = 'S'
            | p `elem` poss = '#'
            | inRange targetArea p = 'T'
            | otherwise = '.'

hits targetArea@((xmin, ymin), (xmax, ymax)) = do
    dx <- [1..xmax]
    dy <- [ymin..(-ymin)]
    let trajectory = shoot targetArea (Probe (dx, dy) (0, 0))
    guard $ any (inRange targetArea . pos) trajectory
    return ((dx, dy), trajectory)

main = do
    let targetArea = ((96, -144), (125, -98))
        hits' = hits targetArea

    -- Part 1
    print $ maximum $ map (snd . pos) $ concatMap snd hits'

    -- Part 2
    print $ length hits' 

