import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data Policy = Policy Int Int Char deriving Show

parsePolicy = Policy <$> number <* char '-' <*> number <* char ' ' <*> get
  where 
    number = read <$> munch1 isDigit

parsePolicyAndPassword = (,) <$> parsePolicy <*> (string ": " *> munch1 (/= '\n'))

valid1 (Policy lo hi x, password) = 
    case find ((== x) . head) (group (sort password)) of
        Nothing -> False
        Just ys -> lo <= length ys && hi >= length ys

valid2 (Policy lo hi x, password) = (password !! (lo - 1) == x) /= (password !! (hi - 1) == x)

main = do
    input <- readFile "input.txt"
    let [(policies, "")] = readP_to_S (sepBy1 parsePolicyAndPassword (char '\n') <* skipSpaces <* eof) input

    -- Part 1
    print $ length $ filter valid1 policies

    -- Part 2
    print $ length $ filter valid2 policies

