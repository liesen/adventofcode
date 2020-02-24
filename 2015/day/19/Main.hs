{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.List

parseReplacement (span isAlpha -> (x, ' ':'=':'>':' ':y)) = (x, y)

generate :: String -> [(String, String)] -> [String]
generate []       _            = []
generate molecule replacements =
    map (head molecule :) (generate (tail molecule) replacements)
    ++ [y ++ drop (length x) molecule | (x, y) <- replacements, x `isPrefixOf` molecule]

main = do
    input <- readFile "input.txt"
    let (map parseReplacement -> replacements, _:molecule:_) = break null (lines input)
    
    -- Part 1
    print $ length $ nub $ generate molecule replacements
