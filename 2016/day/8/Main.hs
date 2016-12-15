{-# LANGUAGE ViewPatterns #-}
import Data.Array
import Data.List

--- Day 8: Two-Factor Authentication ---

type Screen = Array (Int, Int) Bool

screen w h = listArray ((0, 0), (w - 1, h - 1)) (repeat False)

apply scr (break (== ' ') -> ("rect", ' ':args)) = rect scr args
apply scr (break (== ' ') -> ("rotate", ' ':args)) = rotate scr args

rect scr (break (== 'x') -> (x1', 'x':y1')) =
    let x1 = read x1' - 1
        y1 = read y1' - 1
    in scr // [((x, y), True) | x <- [0..x1], y <- [0..y1]]
                
rotate scr (break (== ' ') -> ("row", ' ':args')) = rotateRow scr args'
rotate scr (break (== ' ') -> ("column", ' ':args')) = rotateColumn scr args'

rotateRow scr ('y':'=':args) =
    let (y', ' ':'b':'y':' ':dx') = break (== ' ') args
        y = read y'
        dx = read dx'
        ((x0, y0), (x1, y1)) = bounds scr
        w = x1 - x0 + 1
    in scr // [((x, y), scr ! ((x - dx) `mod` w, y)) | x <- [x0..x1]]

rotateColumn scr ('x':'=':args) =
    let (x', ' ':'b':'y':' ':dy') = break (== ' ') args
        x = read x'
        dy = read dy'
        ((x0, y0), (x1, y1)) = bounds scr
        h = y1 - y0 + 1
    in scr // [((x, y), scr ! (x, (y - dy) `mod` h)) | y <- [y0..y1]]

showScreen scr =
    let ((x0, y0), (x1, y1)) = bounds scr
    in unlines [[if scr ! (x, y) then '#' else '.' | x <- [x0..x1]] | y <- [y0..y1]]

-- test
instructions = [
    "rect 3x2",
    "rotate column x=1 by 1",
    "rotate row y=0 by 4",
    "rotate column x=1 by 1"
  ]

test = putStr $ intercalate (replicate 7 '-' ++ "\n") $ map showScreen $ scanl apply (screen 7 3) instructions

run :: Screen -> [String] -> Screen
run = foldl apply

count :: Screen -> Int
count = length . filter (== True) . elems

-- CFLELOYFCS
main = readFile "input.txt" >>= \input -> do
    let output = run (screen 50 6) (lines input)
    print $ count output
    putStrLn "--- Part Two ---"
    putStr . showScreen $ output
