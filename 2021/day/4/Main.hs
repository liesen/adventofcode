import Data.Char (isDigit)
import Data.List
import Text.ParserCombinators.ReadP

newtype Board = Board [[Int]]
  deriving (Show)

parseNumber = read <$> many1 (satisfy isDigit)

parseRow = count 5 (skipSpaces *> parseNumber)

parseBoard = Board <$> count 5 (parseRow <* char '\n')

data Problem = Problem [Int] [Board]
  deriving (Show)

parseProblem = do
    numbers <- sepBy1 parseNumber (char ',') <* char '\n'
    boards <- many1 (char '\n' *> parseBoard)
    return $ Problem numbers boards

-- Bingo bongo
bingo :: [Int] -> Board -> Bool
bingo numbers (Board rows) = 
    any (bingoRow numbers) (rows ++ transpose rows)
  where
    bingoRow :: [Int] -> [Int] -> Bool
    bingoRow numbers xs = null (xs \\ numbers)

data Bingo = Bingo [Int] Board

bingos :: Problem -> [Bingo]
bingos (Problem numbers boards) =
    concat $ unfoldr f (inits numbers, boards)
  where
    f ([], _) = Nothing
    f (_, []) = Nothing
    f (ns:nss, remainingBoards) =
        let (bingoBoards, remainingBoards') = partition (bingo ns) remainingBoards
        in Just (map (Bingo ns) bingoBoards, (nss, remainingBoards'))

score (Bingo numbers (Board rows)) = sum (concat rows \\ numbers) * last numbers


main = do
    input <- readFile "input.txt"
    let [(problem, "")] = readP_to_S (parseProblem <* skipSpaces <* eof) input

    -- Part 1
    print $ score $ head $ bingos problem

    -- Part 2
    print $ score $ last $ bingos problem

