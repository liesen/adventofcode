import Control.Arrow
import Control.Applicative
import Data.Char
import Data.Function (on)
import Data.List
import Data.Monoid
import Data.Ord
import Text.ParserCombinators.ReadP

-- Vector
data V = V Int Int Int deriving (Show, Eq, Ord)

instance Monoid V where
    mempty = V 0 0 0
    mappend (V x0 y0 z0) (V x1 y1 z1) = V (x0 + x1) (y0 + y1) (z0 + z1)

-- Particle
data P = P { position :: V, velocity :: V, acceleration :: V } deriving (Show, Eq)

instance Monoid P where
    mempty = P mempty mempty mempty
    mappend (P p0 v0 a0) (P p1 v1 a1) = P (p0 <> p1) (v0 <> v1) (a0 <> a1)

-- Update a vector
updateP (P p v a) = let v' = v <> a in P (p <> v') v' a

-- Parsing
sep = skipSpaces >> char ',' >> skipSpaces

vector = between (char '<' >> skipSpaces) (skipSpaces >> char '>') $ do
           x <- num
           sep
           y <- num
           sep
           z <- num
           return $ V x y z
  where
    num = do sign <- option 1 (char '-' >> return (-1))
             n <- read <$> munch1 isDigit
             return (sign * n)

particle = P <$> p <*> v <*> a
  where
    p = (char 'p' *> char '=' *> vector <* sep)
    v = (char 'v' *> char '=' *> vector <* sep)
    a = (char 'a' *> char '=' *> vector)

-- Manhattan distance between two coordinates
distV (V x0 y0 z0) (V x1 y1 z1) = abs (x0 - x1) + abs (y0 - y1) + abs (z0 - z1)

-- Particle distance to a coordinate
distP p0 (P p _ _) = distV p0 p

-- Particle distance to origin
dist0 = distP (V 0 0 0)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

main = do
    input <- readFile "input.txt"
    let [(ps, "")] = readP_to_S (endBy particle skipSpaces <* eof) input
        particles = zip [0..] ps
        tick = map (id *** updateP)

    -- Part 1
    -- Keep ticking until the order of the particles (by distance to the 
    -- origin) does not change
    print $ fst $ head $ converge ((==) `on` map fst) $ iterate (sortBy (comparing (dist0 . snd)) . tick) particles

    -- Part 2
    let resolveCollisions = concat . filter ((== 1) . length) . groupBy ((==) `on` (position . snd)) . sortBy (comparing (position . snd))
    print $ length $ converge ((==) `on` map fst) $ iterate (sortBy (comparing (dist0 . snd)) . resolveCollisions . tick) particles
    -- Unclear if the above termination criterion is actually correct, however
    -- it seems to be good enough.  Same result as when doing 2000 ticks:
    -- print $ length $ (iterate (resolveCollisions . tick) particles) !! 2000

