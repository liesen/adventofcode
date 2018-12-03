import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP


data Rect = 
    Rect { x :: Int
         , y :: Int
         , w :: Int
         , h :: Int
         }
  deriving (Show, Eq)

isEmpty :: Rect -> Bool
isEmpty (Rect _ _ w h) = w == 0 || h == 0

instance Monoid Rect where
    mempty = Rect 0 0 0 0
    mappend (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2)
      = let x = min x1 x2
            y = min y1 y2
        in Rect x
                y
                (max (x1 + w1) (x2 + w2) - x)
                (max (y1 + h1) (y2 + h2) - y)


intersection r@(Rect x1 y1 w1 h1) t@(Rect x2 y2 w2 h2) =
    let tx1 = max x1 x2
        ty1 = max y1 y2
        tx2 = min (x1 + w1) (x2 + w2)
        ty2 = min (y1 + h1) (y2 + h2)
    in if tx1 < tx2 && ty1 < ty2
         then return (Rect tx1 ty1 (tx2 - tx1) (ty2 - ty1))
         else fail "rectangles don't intersect"


data Claim = Claim { claimId :: Int, rect :: Rect }
  deriving (Show, Eq)

parse :: ReadP [Claim]
parse = sepBy parseLine (char '\n') <* optional (char '\n') <* eof

parseLine :: ReadP Claim
parseLine = do
    _ <- char '#'
    i <- num
    _ <- string " @ "
    x <- num
    _ <- char ','
    y <- num
    _ <- string ": "
    w <- num
    _ <- char 'x'
    h <- num
    return $ Claim i (Rect x y w h)
  where
    num :: ReadP Int
    num = read `fmap` many1 (satisfy isDigit)


newtype Fabric = Fabric (Array (Int, Int) Int)

instance Show Fabric where
    show (Fabric a) =
        let ((x0, y0), (x1, y1)) = bounds a
        in unlines [[if c == 0 then '.' else intToDigit c | x <- [x0..x1], let c = a ! (x, y) ] | y <- [y0..y1]]

fill (Fabric a) (Rect x y w h) = Fabric $ accum (+) a (map (\p -> (p, 1)) (range ((x, y), (x + w - 1, y + h - 1))))


main = do
    input <- readFile "input.txt"
    let [(claims, "")] = readP_to_S parse input

    -- Part 1
    let envelope@(Rect x y w h) = mconcat (map rect claims)
        a = Fabric $ listArray ((x, y), (x + w, y + h)) (repeat 0)
        Fabric b = foldl fill a (map rect claims)
        n = length $ filter (> 1) $ elems b
    print n

    -- Part 2
    let [m] = [m | (Claim m x) <- claims,
                   all isNothing [intersection x y | (Claim n y) <- claims, m /= n]]
    print m

