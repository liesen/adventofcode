import Data.Char (isAlpha, isDigit)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP

type ColorCode = String

type Graph = [(ColorCode, [(ColorCode, Integer)])]

colorCode :: ReadP ColorCode
colorCode = do
    adjective <- munch1 isAlpha
    char ' '
    color <- munch1 isAlpha
    return (adjective ++ ' ' : color)

bags :: ReadP (ColorCode, Integer)
bags = do
    n <- read <$> munch1 isDigit
    char ' '
    key <- colorCode
    if n == 1
        then string " bag"
        else string " bags"
    return (key, n)

parseLine :: ReadP (ColorCode, [(ColorCode, Integer)])
parseLine = do
    bag <- colorCode
    string " bags contain "
    contents <- 
        (string "no other bags" *> pure [])
        +++
        (bags `sepBy` string ", ")
    char '.'
    return (bag, contents)

parse :: ReadP Graph
parse = parseLine `endBy` char '\n' <* eof

-- Returns list of vertices reachable from a given vertex.
reachable :: Graph -> ColorCode -> [ColorCode]
reachable g v = v : maybe [] (concatMap (reachable g . fst)) (lookup v g)

countBagsInside :: Graph -> ColorCode -> Integer
countBagsInside g v = sum [n + n * countBagsInside g w | (w, n) <- fromMaybe [] (lookup v g)]

main = do
    input <- readFile "input.txt"
    let [(graph, "")] = readP_to_S parse input

    -- Part 1
    print $ length $ filter (elem "shiny gold" . tail . reachable graph . fst) graph

    -- Part 2
    print $ countBagsInside graph "shiny gold"
