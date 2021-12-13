import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

newtype Paper = Paper (Set (Int, Int))

instance Show Paper where
    show paper@(Paper dots) =
            unlines [ [if Set.member (x, y) dots then '#' else '.' | x <- [0..xmax]]
                    | y <- [0..ymax]
                    ]
        where
            xmax = maximum (Set.map fst dots)
            ymax = maximum (Set.map snd dots)

data PaperFold
    = HFold Int
    | VFold Int
    deriving (Show)

parseNumber = read <$> many1 (satisfy isDigit)

parseDot = (,) <$> parseNumber <*> (char ',' *> parseNumber)

parsePaper = Paper . Set.fromList <$> endBy parseDot (char '\n')

parseFold = hfold +++ vfold
    where
        hfold = HFold <$> (string "fold along y=" *> parseNumber)
        vfold = VFold <$> (string "fold along x=" *> parseNumber)

parse = (,) <$> parsePaper <*> many (char '\n' *> parseFold)

paperFold :: Paper -> PaperFold -> Paper
paperFold paper@(Paper dots) = Paper . flip Set.map dots . f
    where
        f (HFold yy) (x, y) = if y <= yy then (x, y) else (x, yy - (y - yy))
        f (VFold xx) (x, y) = if x <= xx then (x, y) else (xx - (x - xx), y)

countDots (Paper dots) = Set.size dots

main = do
    input <- readFile "input.txt"
    let [((paper, folds), "")] = readP_to_S (parse <* skipSpaces <* eof) input

    -- Part 1
    print $ countDots $ paperFold paper $ head folds

    -- Part 2
    print $ foldl paperFold paper folds
