import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP

parseNumber = read <$> many1 (satisfy isDigit)

parseNumbers = sepBy parseNumber (char ',')

step = Map.unionsWith (+) . Map.mapWithKey spawn

spawn 0 n = Map.fromList [(6, n), (8, n)]
spawn i n = Map.fromList [(i - 1, n)]

main = do
    input <- readFile "input.txt"
    let [(numbers, "")] = readP_to_S (parseNumbers <* skipSpaces <* eof) input
        state0 = Map.fromListWith (+) (zip numbers (repeat 1))
        states = iterate step state0

    -- Part 1
    print $ sum $ states !! 80

    -- Part 2
    print $ sum $ states !! 256

