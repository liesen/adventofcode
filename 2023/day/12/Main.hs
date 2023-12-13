import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP

data Record = Rec String [Int] Int deriving (Eq, Ord, Show)

parseRecord = Rec <$> many1 (satisfy (`elem` ".?#")) <*> (char ' ' *> sepBy number (char ',')) <*> pure 0

number :: ReadP Int
number = read <$> munch1 isDigit

parse = endBy parseRecord (char '\n') <* eof

unfold (Rec s g 0) = Rec (intercalate "?" (replicate 5 s)) (concat (replicate 5 g)) 0

arrs :: Map Record Integer -> Record -> (Integer, Map Record Integer)
arrs memo k@(Rec s gs gn) =
  case Map.lookup k memo of
    Just a -> (a, memo)
    Nothing ->
      case k of
        Rec s [] _ -> (if '#' `notElem` s then 1 else 0, memo)
        Rec "" (n : ns) m ->
          if n == m
            then let (a, memo') = arrs memo (Rec "" ns 0) in (a, Map.insert k a memo')
            else (0, memo)
        Rec ('.' : s') ns 0 ->
          let (a, memo') = arrs memo (Rec s' ns 0)
           in (a, Map.insert k a memo')
        Rec ('.' : s') (n : ns) m ->
          if n == m
            then let (a, memo') = arrs memo (Rec s' ns 0) in (a, Map.insert k a memo')
            else (0, memo)
        Rec ('#' : s') (n : ns) m ->
          if m < n
            then let (a, memo') = arrs memo (Rec s' (n : ns) (m + 1)) in (a, Map.insert k a memo')
            else (0, memo)
        Rec ('?' : s') ns m ->
          let (a, memo') = arrs memo (Rec ('.' : s') ns m)
              (b, memo'') = arrs memo' (Rec ('#' : s') ns m)
           in (a + b, Map.insert k (a + b) memo'')

main = do
  input <- readFile "input"
  let [(cases, "")] = readP_to_S parse input
  print $ sum $ map (fst . arrs mempty) cases
  print $ sum $ map (fst . arrs mempty . unfold) cases
