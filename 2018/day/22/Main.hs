import Control.Applicative
import Data.Array
import Data.Char
import Text.ParserCombinators.ReadP


data Cave = Cave
    { depth :: Int
    , target :: (Int, Int)
    } deriving (Eq, Show)

parse :: ReadP Cave
parse = do  
    depth <- string "depth: " *> number
    _ <- char '\n'
    target <- string "target: " *> liftA2 (,) number (char ',' *> number)
    _ <- char '\n'
    return $ Cave depth target
  where
    number = read <$> munch1 isDigit

test = Cave 510 (10, 10)

geologicIndex (Cave depth target@(x1, y1)) = a
  where
    mouth = ((0, 0), 0)
    normalize x = ((x + depth) `mod` 20183)
    x0s = [((0, y), normalize (fromIntegral y * 48271)) | y <- [0..y1]]
    y0s = [((x, 0), normalize (fromIntegral x * 16807)) | x <- [0..x1]]
    a = array ((0, 0), target) (mouth : x0s ++ y0s ++ [((x, y), normalize (a ! (x - 1, y) * a ! (x, y - 1))) | (x, y) <- range ((1, 1), (x1, y1))])

showCave cave@(Cave depth target) =
    unlines [[draw (x, y) | x <- [x0..x1]]
            | y <- [y0..y1]
            ]
  where
    a = geologicIndex cave
    bnds@((x0, y0), (x1, y1)) = bounds a
    draw (0, 0) = 'M'
    draw xy | xy == target = 'T'
            | otherwise    = case (a ! xy) `mod` 3 of
                                 0 -> '.'
                                 1 -> '='
                                 2 -> '|'

riskLevel cave = sum (elems b) - b ! (0, 0) - b ! (target cave)
  where
    a = geologicIndex cave
    b = fmap (`mod` 3) a

main = do
    input <- readFile "input.txt"
    let [(cave, "")] = readP_to_S (parse <* eof) input

    -- Part 1
    print $ riskLevel cave
