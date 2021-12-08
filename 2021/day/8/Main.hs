import Text.ParserCombinators.ReadP
import Data.Char

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

main = do
    input <- readFile "input.txt"
    let [(problems, "")] = readP_to_S (parseProblems <* skipSpaces <* eof) input

    -- Part 1
    print $ length $ filter (`elem` [2, 3, 4, 7]) $ concatMap (map length . outputs) problems
