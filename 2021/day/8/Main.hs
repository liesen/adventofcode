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

solve2 (Problem inputs outputs) =
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
    print $ fromJust $ sum <$> mapM solve2 problems
