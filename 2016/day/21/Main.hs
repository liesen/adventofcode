{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.List

rotateLeft s i = let (x, y) = splitAt (i `mod` length s) s in y ++ x

rotateRight s = rotateLeft s . negate

scramble :: String -> String -> String
scramble s (words -> ["swap","position",read -> x,"with","position",read -> y]) = 
    let (as, splitAt (max x y - min x y) -> (b:bs, c:cs)) = splitAt (min x y) s
    in concat [as, [c], bs, [b], cs]
scramble s (words -> ["swap","letter",head -> x,"with","letter",head -> y]) =
    map (\z -> if z == x then y else if z == y then x else z) s
scramble s (words -> ["rotate","left",read -> x,"step"]) = rotateLeft s x
scramble s (words -> ["rotate","left",read -> x,"steps"]) = rotateLeft s x
scramble s (words -> ["rotate","right",read -> x,"step"]) = rotateRight s x
scramble s (words -> ["rotate","right",read -> x,"steps"]) = rotateRight s x
scramble s (words -> ["rotate","based","on","position","of","letter",head -> x]) =
    case elemIndex x s of
       Nothing         -> s
       Just i | i >= 4 -> rotateRight s (1 + i + 1)
       Just i          -> rotateRight s (1 + i)
scramble s (words -> ["reverse","positions",read -> x,"through",read -> y]) =
    let (as, splitAt (max x y - min x y + 1) -> (bs, cs)) = splitAt (min x y) s
    in concat [as, reverse bs, cs]
scramble s (words -> ["move","position",read -> x,"to","position",read -> y])
      | x < y     = concat [as, bs, [c], [b], cs]
      | x > y     = concat [as, [c], [b], bs, cs]
      | otherwise = s
    where
      (splitAt (min x y) -> (as, b:bs), c:cs) = splitAt (max x y) s

test1 = zip ("":input) (scanl scramble "abcde" input)
  where
    input = 
      [
        "swap position 4 with position 0",
        "swap letter d with letter b",
        "reverse positions 0 through 4",
        "rotate left 1 step",
        "move position 1 to position 4",
        "move position 3 to position 0",
        "rotate based on position of letter b",
        "rotate based on position of letter d"
      ]

main1 = do
    input <- lines `fmap` readFile "input.txt"
    -- mapM_ print $ zip ("":input) (scanl step "abcdefgh" input)
    putStrLn $ foldl scramble "abcdefgh" input

--- Part Two ---

main2 = do
    program <- lines `fmap` readFile "input.txt"
    let Just input = find (\s -> foldl scramble s program == "fbgdceah") (permutations "abcdefgh")
    putStrLn input

main = main1 >> main2
