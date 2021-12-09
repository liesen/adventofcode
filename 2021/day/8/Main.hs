import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

data Problem = Problem { inputs :: [String], outputs :: [String] }
  deriving (Show)

parseSegment :: ReadP String
parseSegment = many1 (satisfy isAlpha)

parseSegments :: ReadP [String]
parseSegments = sepBy1 parseSegment (char ' ')

parseProblem :: ReadP Problem
parseProblem = Problem <$> parseSegments <*> (string " | " *> parseSegments)

parseProblems :: ReadP [Problem]
parseProblems = sepBy1 parseProblem (char '\n')

ex = Problem inputs outputs
  where
    inputs = words "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
    outputs = words "cdfeb fcadb cdfeb cdbaf"

-- Find a lookup table that produces valid numbers for every
-- input string by trying all permutations. Assumes that there is only one
-- such permutation.
brute :: Problem -> Maybe Int
brute (Problem inputs outputs) = listToMaybe $ do
    p <- permutations "abcdefg"
    let table = zip p "abcdefg"
        decode = mapM (`lookup` table)
        encode = flip lookup numbersTable . sort

    case mapM (decode >=> encode) inputs of
        Nothing -> fail "not a valid permutation for inputs"
        Just _ -> case mapM (decode >=> encode) outputs of
                      Nothing -> fail "not a valid permutation for outputs"
                      Just xs -> return $ read $ map intToDigit xs

-- Analytically find a lookup table that is able to unique translate
-- inputs to valid numbers. Assumes that there are numbers in the 
-- input that unique determines such a table (ones, fours, sevens, etc).
clever :: Problem -> Maybe Int
clever (Problem inputs outputs) =
    let [a] = foldr1 union (filter ((== 3) . length) inputs) \\ foldr1 union (filter ((== 2) . length) inputs)
        adg = foldr1 intersect (filter ((== 5) . length) inputs)
        dg = adg \\ [a]
        [g] = dg \\ foldr1 union (filter ((== 4) . length) inputs)
        [d] = dg \\ [g]
        bd = foldr1 union (filter ((== 4) . length) inputs) \\ foldr1 union (filter ((== 2) . length) inputs)
        [b] = bd \\ [d]
        abcdfg = foldr1 union (filter ((== 2) . length) inputs) `union` [a, b, d, g]
        [e] = "abcdefg" \\ abcdfg
        abdefg = foldr1 union (filter (\x -> length x == 6 && e `elem` x && d `elem` x) inputs)
        [c] = "abcdefg" \\ abdefg
        [f] = "abcdefg" \\ [a, b, c, d, e, g]
        table = zip [a, b, c, d, e, f, g] "abcdefgh"
        decode = mapM (`lookup` table)
    in read <$> map intToDigit <$> mapM (decode >=> encode) outputs
  where
    encode = flip lookup numbersTable . sort

numbersTable = [
        ("abcefg", 0),
        ("cf", 1),
        ("acdeg", 2),
        ("acdfg", 3),
        ("bcdf", 4),
        ("abdfg", 5),
        ("abdefg", 6),
        ("acf", 7),
        ("abcdefg", 8),
        ("abcdfg", 9)
    ]

main = do
    input <- readFile "input.txt"
    let [(problems, "")] = readP_to_S (parseProblems <* skipSpaces <* eof) input

    -- Part 1
    print $ length $ filter (`elem` [2, 3, 4, 7]) $ concatMap (map length . outputs) problems

    -- Part 2
    print $ fromJust $ sum <$> mapM clever problems
