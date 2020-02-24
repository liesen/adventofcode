{-# LANGUAGE RecordWildCards #-}
import Data.Char
import Text.ParserCombinators.ReadP

data Sue = Sue
    { index :: Int
    , attrs :: [(String, Int)]
    } deriving (Eq, Show)

parseSue = do
    index <- read <$> (string "Sue " *> munch1 isDigit <* string ": ")
    attrs <- sepBy attr (string ", ")
    return Sue{..}
  where
    attr = do
        s <- manyTill (satisfy isLower) (char ':')
        _ <- char ' '
        n <- read <$> munch1 isDigit
        return (s, n)

parse = endBy parseSue (char '\n') <* eof

main = do
    input <- readFile "input.txt"
    let [(sues, "")] = readP_to_S parse input

    -- Part 1
    mapM_ (print . index)
        $ filter (maybe True (== 3) . lookup "children" . attrs)
        $ filter (maybe True (== 7) . lookup "cats" . attrs)
        $ filter (maybe True (== 2) . lookup "samoyeds" . attrs)
        $ filter (maybe True (== 3) . lookup "pomeranians" . attrs)
        $ filter (maybe True (== 0) . lookup "akitas" . attrs)
        $ filter (maybe True (== 0) . lookup "vizslas" . attrs)
        $ filter (maybe True (== 5) . lookup "goldfish" . attrs)
        $ filter (maybe True (== 3) . lookup "trees" . attrs)
        $ filter (maybe True (== 2) . lookup "cars" . attrs)
        $ filter (maybe True (== 1) . lookup "perfumes" . attrs)
        $ sues

    -- Part 2
    mapM_ (print . index)
        $ filter (maybe True (== 3) . lookup "children" . attrs)
        $ filter (maybe True (> 7) . lookup "cats" . attrs)
        $ filter (maybe True (== 2) . lookup "samoyeds" . attrs)
        $ filter (maybe True (< 3) . lookup "pomeranians" . attrs)
        $ filter (maybe True (== 0) . lookup "akitas" . attrs)
        $ filter (maybe True (== 0) . lookup "vizslas" . attrs)
        $ filter (maybe True (< 5) . lookup "goldfish" . attrs)
        $ filter (maybe True (> 3) . lookup "trees" . attrs)
        $ filter (maybe True (== 2) . lookup "cars" . attrs)
        $ filter (maybe True (== 1) . lookup "perfumes" . attrs)
        $ sues