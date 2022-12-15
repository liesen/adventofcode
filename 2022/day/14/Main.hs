{-# LANGUAGE ImportQualifiedPost, TupleSections, StrictData #-}
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Text.ParserCombinators.ReadP


parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseSegment :: ReadP (Int, Int)
parseSegment = (,) <$> parseNumber <*> (char ',' *> parseNumber)

parseSegments :: ReadP [(Int, Int)]
parseSegments = parseSegment `sepBy1` string " -> "

parse = parseSegments `sepBy` char '\n'

expandSegments (x:xs) = concat . snd $ mapAccumL (\p q -> (q, range (p, q))) x xs

range ((x1, y1), (x2, y2))
    | x1 == x2 = map (x1,) [min y1 y2..max y1 y2]
    | y1 == y2 = map (,y1) [min x1 x2..max x1 x2]

data Cave = Cave { ymax :: Int, rock :: Set (Int, Int), sand :: Set (Int, Int) }

instance Show Cave where
    show (Cave ymax rock sand) =
        unlines [[char (x, y) | x <- [xmin..xmax]] | y <- [ymin..ymax + 2]]        
      where
        xmin = minimum (Set.map fst rock) - 10
        xmax = maximum (Set.map fst rock) + 10
        ymin = 0
        char p@(x, y)
            | p `elem` rock = '#'
            | y == ymax + 2 = '#'
            | p `elem` sand = 'o'
            | otherwise = '.'

pour1 p@(x, y) cave@(Cave ymax rock sand)
    | p `elem` rock || p `elem` sand = fail "hit rock or resting sand"
    | y < ymax  = down <|> left <|> right <|> stop
    | otherwise = stop  -- Pouring into the abyss
  where
    down = pour1 (x, y + 1) cave
    left = pour1 (x - 1, y + 1) cave
    right = pour1 (x + 1, y + 1) cave
    stop = pure (Cave ymax rock (Set.insert p sand))

done1 (Cave ymax rock sand)
    | Set.null sand = False
    | otherwise = maximum (Set.map snd sand) >= ymax

pour2 p@(x, y) cave@(Cave ymax rock sand)
    | p `elem` rock || p `elem` sand = fail "hit rock or resting sand"
    | y >= ymax + 2 = fail "hit the floor"
    | otherwise = down <|> left <|> right <|> stop
    where
        down = pour2 (x, y + 1) cave
        left = pour2 (x - 1, y + 1) cave
        right = pour2 (x + 1, y + 1) cave
        stop = pure (Cave ymax rock (Set.insert p sand))
        
done2 (Cave ymax rock sand) = (500,0) `elem` sand

main = do
    input <- readFile "input.txt"
    let [(segments, "")] = readP_to_S (parse <* skipSpaces <* eof) input
        rock = foldMap (Set.fromList . expandSegments) segments
        ymax = maximum (Set.map snd rock)
        cave = Cave ymax rock mempty
        
    -- Part 1
    let Cave _ _ sand2 = until done1 (fromJust . pour1 (500,0)) cave
    print $ length sand2 - 1  -- Adjust for first unit of sand in the abyss
    
    -- Part 2
    let Cave _ _ sand2 = until done2 (fromJust . pour2 (500,0)) cave
    print $ length sand2