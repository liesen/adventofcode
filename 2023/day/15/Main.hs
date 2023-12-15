import Data.Char
import Data.Map (Map)
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP

hash :: String -> Int
hash = foldl f 0
  where
    f cur ch = ((cur + ord ch) * 17) `mod` 256

split [] = []
split xs = case span (/= ',') xs of
  (y, ',' : ys) -> y : split ys
  (y, []) -> [takeWhile (not . isSpace) y]

data Step
  = Dash String Int
  | Equals String Int Int
  deriving (Eq, Ord)

instance Show Step where
  show (Dash label _) = label ++ "-"
  show (Equals label f _) = label ++ "=" ++ show f

parseStep = do
  s <- many1 (satisfy (`notElem` "-="))
  op <- satisfy (`elem` "-=")

  if op == '-'
    then pure (Dash s (hash s))
    else do
      f <- read <$> munch1 isDigit
      pure (Equals s f (hash s))

data Lens = Lens {lensLabel :: String, focalLength :: Int} deriving (Eq, Ord, Show)

step :: Map Int [Lens] -> Step -> Map Int [Lens]
step boxes (Dash stepLabel stepHash) = Map.adjust (filter ((/= stepLabel) . lensLabel)) stepHash boxes
step boxes x@(Equals l f h) =
  case Map.lookup h boxes of
    Just lenses ->
      case break ((== l) . lensLabel) lenses of
        (as, b : bs) -> Map.insert h (as ++ Lens l f : bs) boxes
        (as, []) -> Map.insert h (as ++ [Lens l f]) boxes
    Nothing -> Map.insert h [Lens l f] boxes

focalPower boxes =
  sum
    [ (boxNum' + 1) * slotNum * focalLength
      | (boxNum', lenses) <- Map.assocs boxes,
        (slotNum, Lens _ focalLength) <- zip [1 ..] lenses
    ]

main = do
  input <- readFile "input"

  -- Part 1
  print $ sum $ map hash $ split input

  -- Part 2
  let [(steps, "")] = readP_to_S (sepBy parseStep (char ',') <* skipSpaces <* eof) input
  print $ focalPower $ foldl step mempty steps