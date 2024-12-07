import Data.Char (isDigit)
import Data.List
import Text.ParserCombinators.ReadP

number :: ReadP Int
number = read <$> many1 (satisfy isDigit)

orderingRule = (,) <$> number <*> (char '|' *> number)

pages = sepBy1 number (char ',')

parse = do
  rs <- endBy orderingRule (char '\n')
  _ <- char '\n'
  ps <- endBy pages (char '\n')
  eof
  return (rs, ps)

ordered orderingRules pages = all (`elem` orderingRules) (zip pages (drop 1 pages))

main = do
  input <- readFile "input"
  let [((orderingRules, updates), "")] = readP_to_S parse input
  let (correct, wrong) = partition (ordered orderingRules) updates

  -- Part 1
  let middlePageNumber pages = pages !! (length pages `div` 2)
  print $ sum $ map middlePageNumber correct

  -- Part 2
  let cmp p1 p2
        | (p1, p2) `elem` orderingRules = LT
        | (p2, p1) `elem` orderingRules = GT
        | otherwise = error "partial ordering rules"
  print $ sum $ map (middlePageNumber . sortBy cmp) wrong
