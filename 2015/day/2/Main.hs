{-# LANGUAGE ViewPatterns #-}
import Data.Monoid (Sum(..))

parse :: String -> (Int, Int, Int)
parse (break (== 'x') -> (read -> l, 'x':(break (== 'x') -> (read -> w, 'x':(read -> h))))) = (l, w, h)

wrappingPaper (parse -> (l, w, h)) =
    let halfsides = [l * w, w * h, h * l]
        sides = map (2 *) halfsides
        slack = minimum halfsides
    in sum sides + slack

ribbon (parse -> (l, w, h)) =
    let wrap = 2 * (l + w + h - max l (max w h))
        bow = l * w * h
    in wrap + bow

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ getSum $ foldMap (Sum . wrappingPaper) (lines input)

    -- Part 2
    print $ getSum $ foldMap (Sum . ribbon) (lines input)
