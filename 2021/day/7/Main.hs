import Data.Char
import Text.ParserCombinators.ReadP

parseNumber :: ReadP Int
parseNumber = read <$> many1 (satisfy isDigit)

parseNumbers :: ReadP [Int]
parseNumbers = sepBy parseNumber (char ',')

cost1, cost2 :: Int -> [Int] -> Int
cost1 i = sum . map (abs . (i -))
cost2 i = sum . map abs . map f
  where
    f x = n * (n + 1) `div` 2
      where n = abs (x - i)

main = do
    input <- getContents
    let [(xs, "")] = readP_to_S (parseNumbers <* skipSpaces <* eof) input
        n = length xs

    -- Part 1
    print $ minimum $ map (flip cost1 xs) [0..n]  -- = the median

    -- Part 2
    print $ minimum $ map (flip cost2 xs) [0..n]
