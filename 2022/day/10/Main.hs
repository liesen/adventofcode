{-# LANGUAGE ViewPatterns #-}
import Data.List
import Control.Arrow

execute :: [String] -> (Int, [Int])
execute = second concat . mapAccumL op 1

op :: Int -> String -> (Int, [Int])
op x "noop" = (x, [x])
op x (stripPrefix "addx " -> Just (read -> dx)) = (x + dx, [x, x])
op x _ = error "bad input"

data Crt = Crt [Bool]
    
draw :: [Int] -> Crt
draw = Crt . take (6 * 40) . zipWith lit (cycle [0..39])
  where
    lit sprite x = x `elem` [sprite - 1, sprite, sprite + 1]

instance Show Crt where
    show (Crt xs) = intercalate "\n" $ map line [0..5]
      where
        line r = map char $ take 40 $ drop (40 * r) xs
        char True = '#'
        char False = ' '

main = do
    input <- readFile "input.txt"
    let (x, signal) = execute (lines input)
    
    -- Part 1
    print $ sum $ map (\cycle -> cycle * (signal !! (cycle - 1))) [20, 60, 100, 140, 180, 220]
    
    -- Part 2
    let crt = draw signal
    print crt
