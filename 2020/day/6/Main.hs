import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

parse = munch1 isAlpha `endBy` char '\n' `sepBy` char '\n'

main = do
    input <- readFile "input.txt"
    let [(groups, "")] = readP_to_S (parse <* eof) input

    -- Part 1
    print $ length $ concatMap (foldr union []) groups

    -- Part 2
    print $ length $ concatMap (foldr1 intersect) groups
