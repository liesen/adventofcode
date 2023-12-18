import Data.Char
import Data.Functor
import Numeric (readHex)
import Text.ParserCombinators.ReadP

data V = V !Int !Int deriving (Eq, Ord, Show)

V y x .* n = V (n * y) (n * x)

V y x .+ V dy dx = V (y + dy) (x + dx)

data Dir = U | D | L | R deriving (Eq, Ord, Show)

direction U = V (-1) 0
direction D = V 1 0
direction L = V 0 (-1)
direction R = V 0 1

data Step = Step Dir Int String deriving (Eq, Ord, Show)

parseStep :: ReadP Step
parseStep = do
  dir <- choice [char 'U' $> U, char 'D' $> D, char 'L' $> L, char 'R' $> R]
  char ' '
  len <- read <$> many1 (satisfy isDigit)
  hex <- between (string " (") (char ')') (char '#' *> count 6 (satisfy (`elem` "0123456789abcdef")))
  pure (Step dir len hex)

step p (Step dir len hex) = p .+ (direction dir .* len)

-- Trapezoid formula for polygon area
polyArea vs = a2 `div` 2
  where
    a2 = sum $ zipWith (\(V y1 x1) (V y2 x2) -> (y1 + y2) * (x1 - x2)) vs (tail vs)

polyBoundary vs = sum $ zipWith dist vs (tail vs)
  where
    dist (V y1 x1) (V y2 x2) = abs (y1 - y2) + abs (x1 - x2)

parseHex p@(Step d n s) = Step d' n' ""
  where
    (ns, [dc]) = splitAt 5 s
    [(n', "")] = readHex ns
    d' = [R, D, L, U] !! digitToInt dc

main = do
  input <- readFile "input"
  let [(steps, "")] = readP_to_S (sepBy parseStep (char '\n') <* skipSpaces <* eof) input

  -- Part 1
  let poly = scanl step (V 0 0) steps
      a = polyArea poly
      b = polyBoundary poly
      i = a + 1 - b `div` 2 -- Pick's theorem
  print (b + i)

  -- Part 2
  let poly = scanl step (V 0 0) $ map parseHex steps
      a = polyArea poly
      b = polyBoundary poly
      i = a + 1 - b `div` 2
  print (b + i)
