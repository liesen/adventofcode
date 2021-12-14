import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP

data Problem = Problem
    { polymer :: String
    , rules :: Map (Char, Char) Char
    }
    deriving (Show)

parseChar = satisfy isUpper

-- parseRule :: ReadP ((Char, Char), Char)
parseRule = do
        x <- (,) <$> parseChar <*> parseChar
        string " -> "
        y <- parseChar
        return (x, y)

parseRules = Map.fromList <$> sepBy parseRule (char '\n')

parseProblem = Problem <$> parseTemplate <*> (string "\n\n" *> parseRules)
    where parseTemplate = many1 parseChar

-- Update polymer string based on the rules. Slow.
slowStep :: Problem -> Problem
slowStep (Problem polymer rules) = Problem polymer' rules
    where
        xs = mapMaybe (`Map.lookup` rules) (zip polymer (tail polymer))
        polymer' = merge polymer xs
        merge (x:xs) []     = x:xs
        merge (x:xs) (y:ys) = x:y:merge xs ys

type Counter a = Map (Char, Char) a

-- Update count of character pairs based on the rules. Fast.
fastStep :: Num a => Problem -> Counter a -> Counter a
fastStep (Problem _ rules) = Map.foldlWithKey apply mempty
    where
        apply c k@(x, z) n =
            case Map.lookup k rules of
                Nothing -> Map.insertWith (+) k n c
                Just y -> Map.insertWith (+) (x, y) n $ Map.insertWith (+) (y, z) n c

steps :: (Ord a, Num a) => Problem -> [a]
steps problem@(Problem polymer@(p:olymer) _) =
        map (score . summarize) $ iterate (fastStep problem) init
    where
        init = Map.fromListWith (+) (zip (zip polymer olymer) (repeat 1))
        summarize = Map.insertWith (+) p 1 . Map.mapKeysWith (+) snd
        score summary = maximum summary - minimum summary

main = do
    input <- readFile "input.txt"
    let [(problem, "")] = readP_to_S (parseProblem <* skipSpaces <* eof) input

    -- Part 1
    print $ steps problem !! 10

    
    -- Part 2
    print $ steps problem !! 40

