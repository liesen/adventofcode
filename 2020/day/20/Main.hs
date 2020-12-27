{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}
import Control.Monad
import Data.Bits
import Data.Char
import Data.List (transpose, delete, find, foldl', (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Prelude hiding (init)
import Text.ParserCombinators.ReadP

xmin = 0
ymin = 0
xmax = 9
ymax = 9

imageSize = 10

data Im = Im
    { topEdge :: !Int
    , bottomEdge :: !Int
    , leftEdge :: !Int
    , rightEdge :: !Int
    }
  deriving (Eq, Ord, Show)

edges (Im t b l r) = [t, b, l, r]

rev n = sum [setBit 0 (imageSize - 1 - i) | i <- [0..imageSize - 1], testBit n i]

rotateRight (Im top bottom left right) = Im (rev left) (rev right) bottom (rev top)
rotateLeft (Im top bottom left right) = Im right left (rev top) bottom
flipVertical (Im top bottom left right) = Im (rev top) (rev bottom) right left
flipHorizontal (Im top bottom left right) = Im bottom top (rev left) (rev right)

transforms = do
    rotate <- [id, rotateLeft, rotateRight]
    flip <- [id, flipVertical, flipHorizontal]
    return (rotate . flip)

{-
instance Show Im where
  show (Im top bottom left right) =
      unlines $ [topLine] ++ body ++ [bottomLine]
    where
      topLine = [if testBit top (xmax - i) then '#' else '.' | i <- [0..xmax]]
      body = [[if testBit left (ymax - i) then '#' else '.'] ++ "        " ++ [if testBit right i then '#' else '.'] | i <- [1..ymax - 1]]
      bottomLine = [if testBit bottom (xmax - i) then '#' else '.' | i <- [0..xmax]]
-}

image :: ReadP Im
image = do
    lines <- count 10 (count 10 (satisfy (`elem` ".#")) <* char '\n')
    let t = side (lines !! 0)
        b = side (lines !! ymax)
        l = side (transpose lines !! 0)
        r = side (transpose lines !! xmax)
    return (Im t b l r)

side = foldl setBit 0 . map fst . filter ((== '#') . snd) . zip [9,8..0]

decimal = read <$> munch1 isDigit

type Tile = (Int, Im)

tile :: ReadP Tile
tile = (,) <$> (string "Tile " *> decimal <* string ":\n")
           <*> image

tiles = tile `sepBy` char '\n'

data State = State
    { solved :: Map (Int, Int) (Int, Im)
    , unsolved :: [Int]
    }

astars :: Ord r
       => (a -> r)  -- ^ Representation
       -> (a -> [(a, Int)])  -- ^ Neighbor function (neighbor node, step cost)
       -> (a -> Int)  -- ^ Heuristic function (if const 0 then astar = bfs)
       -> [a]  -- ^ Start nodes
       -> [(a, Int)]
astars rep next heur starts = loop Set.empty q0
  where
    q0 = PQueue.fromList $ map (\start -> (heur start, (start, 0))) starts
    loop _    (PQueue.minView -> Nothing) = []
    loop seen (PQueue.minView -> Just ((x, cost), q1))
        | Set.member r seen = loop seen q1
        | otherwise = (x, cost) : loop seen1 q2
        where
          r = rep x
          seen1 = Set.insert r seen
          q2 = foldl' (\q (x', stepcost) -> let cost' = cost + stepcost
                                            in cost' `seq` PQueue.insert (cost' + heur x') (x', cost') q)
                      q1
                      (next x)

rep (State solved unsolved) = solved

heur size (State solved unsolved) = length unsolved

init :: [Tile] -> [State]
init unsolved = do
    (n, im) <- unsolved
    f <- transforms
    let im' = f im
        solved' = Map.singleton (0, 0) (n, im')
        unsolved' = delete n (map fst unsolved)
    return $ State solved' unsolved'

next :: Bool -> Int -> Map Int Im -> State -> [(State, Int)]
next part1 size images (State solved unsolved) = do
    (y, x) <- Map.keys solved
    p@(y', x') <- [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
    guard $ y' >= 0 && y' <= size && x' >= 0 && x' <= size
    guard $ p `Map.notMember` solved

    -- We only care about the corners => only fill edges
    when part1 $ guard $ x' == 0 || x' == size || y' == 0 || y' == size
    n <- unsolved
    let Just im = Map.lookup n images
    f <- transforms
    let im' = f im
    guard $ all (\test -> test p im') [testAbove, testBelow, testLeft, testRight]
    let solved' = Map.insert p (n, im') solved
        unsolved' = delete n unsolved
    return $ (State solved' unsolved', 1)
  where
    testAbove (y, x) im = maybe True ((topEdge im ==) . bottomEdge . snd) (Map.lookup (y - 1, x) solved)
    testBelow (y, x) im = maybe True ((bottomEdge im ==) . topEdge . snd) (Map.lookup (y + 1, x) solved)
    testLeft (y, x) im = maybe True ((leftEdge im ==) . rightEdge . snd) (Map.lookup (y, x - 1) solved)
    testRight (y, x) im = maybe True ((rightEdge im ==) . leftEdge . snd) (Map.lookup (y, x + 1) solved)

corners size = [(0, 0), (0, size), (size, 0), (size, size)]

done1 size (State solved unsolved) = all (`Map.member` solved) (corners size)

done2 size (State solved unsolved) = null unsolved

main = do
    input <- readFile "input.txt"
    let [(images, "")] = readP_to_S (tiles <* skipSpaces <* eof) input
        size = truncate (sqrt (fromIntegral (length images))) - 1

    -- Find edge images
    let edgeIds = Set.unions
                $ filter ((== 1) . Set.size)
                $ Map.elems
                $ Map.fromListWith Set.union [(e, Set.singleton n) | (n, im) <- images, f <- transforms, let im' = f im, e <- edges im']
        edgeIms = filter ((`Set.member` edgeIds) . fst) images

    case find (done1 size . fst) (astars rep (next True size (Map.fromList edgeIms)) (heur size) (init edgeIms)) of
        Nothing -> putStrLn "no solution"
        Just ((State solved unsolved), m) -> do
            print $ product $ map fst $ mapMaybe (`Map.lookup` solved) $ corners size
