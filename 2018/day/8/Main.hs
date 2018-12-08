import Data.Char
import Data.Tree
import Text.ParserCombinators.ReadP


parseTree :: ReadP (Tree [Int])
parseTree = do
    numChildNodes <- num
    _ <- sep
    numMetadataEntries <- num
    childNodes <- count numChildNodes (sep >> parseTree)
    metadataEntries <- count numMetadataEntries (sep >> num)
    return $ Node metadataEntries childNodes
  where
    num :: ReadP Int
    num = read <$> many1 (satisfy isDigit)

    sep = char ' '

check2 (Node xs []) = sum xs
check2 (Node xs ys) = sum $ map (maybe 0 id . flip lookup (zip [1..] (map check2 ys))) xs

main = do
    input <- readFile "input.txt"
    let [(tree, "")] = readP_to_S (parseTree <* char '\n' <* eof) input

    -- Part 1
    print $ sum $ fmap sum tree

    -- Part 2
    print $ check2 tree
