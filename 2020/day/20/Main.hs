{-# LANGUAGE ImportQualifiedPost, ViewPatterns #-}
import Control.Monad
import Data.Char
import Data.List hiding (init)
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


data Im = Im { pixels :: Set (Int, Int) }
  deriving (Eq, Ord)

topEdge (Im ps) = [Set.member (0, x) ps | x <- [0..9]]
bottomEdge (Im ps) = [Set.member (9, x) ps | x <- [0..9]]
leftEdge (Im ps) = [Set.member (y, 0) ps | y <- [0..9]]
rightEdge (Im ps) = [Set.member (y, 9) ps | y <- [0..9]]

edges im = [topEdge im, bottomEdge im, leftEdge im, rightEdge im]

rotateRight (Im ps) = Im $ Set.map (\(y, x) -> (x, ymax - y)) ps
  where ymax = maximum (Set.map fst ps)
rotateLeft (Im ps) = Im $ Set.map (\(y, x) -> (xmax - x, y)) ps
  where xmax = maximum (Set.map snd ps)
flipVertical (Im ps) = Im $ Set.map (\(y, x) -> (y, xmax - x)) ps
  where xmax = maximum (Set.map snd ps)
flipHorizontal (Im ps) = Im $ Set.map (\(y, x) -> (ymax - y, x)) ps
  where ymax = maximum (Set.map fst ps)

transforms = do
    rotate <- [id, rotateLeft]
    flip <- [id, flipVertical, flipHorizontal, flipVertical . flipHorizontal]
    return (rotate . flip)

instance Show Im where
  show (Im ps) = unlines [ [if (y, x) `Set.member` ps then '#' else ' ' | x <- [xmin..xmax] ]
                         | y <- [ymin..ymax]]
    where
      ymax = maximum (Set.map fst ps)
      xmax = maximum (Set.map snd ps)
      ymin = minimum (Set.map fst ps)
      xmin = minimum (Set.map snd ps)

image :: ReadP Im
image = do
    lines <- count imageSize (count imageSize (satisfy (`elem` ".#")) <* char '\n')
    return $ Im $ Set.fromList [(y, x) | (y, xs) <- zip [0..] lines, (x, '#') <- zip [0..] xs]
  where
    imageSize = 10

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

astar :: Ord r
       => (a -> r)  -- ^ Representation
       -> (a -> [(a, Int)])  -- ^ Neighbor function (neighbor node, step cost)
       -> (a -> Int)  -- ^ Heuristic function (if const 0 then astar = bfs)
       -> [a]  -- ^ Start nodes
       -> [(a, Int)]
astar rep next heur starts = loop Set.empty q0
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
next part1 size images (State solved unsolved) = do -- traceShow (length solved) $ do
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

assemble :: Map (Int, Int) (Int, Im) -> Im
assemble = 
    Im
    . Set.fromList
    . concatMap (\((yy, xx), (_, (Im ps))) ->
        [ (yy * 8 + y - 1, xx * 8 + x - 1)
        | p@(y, x) <- Set.toList ps
        , y /= 0 && y /= 9 && x /= 0 && x /= 9
        ]
    )
    . Map.assocs

seaMonster = Set.fromList [(y, x) | (y, xs) <- zip [0..] ys, (x, '#') <- zip [0..] xs]
  where
    ys = [ "                  # "
         , "#    ##    ##    ###"
         , " #  #  #  #  #  #   "
         ]

removeSeaMonsters (Im ps) =
    foldl remove ps $ do
        -- Rotate sea monster and try to match it
        -- to the image
        y <- [ymin..ymax]
        x <- [xmin..xmax]
        f <- transforms
        return $ translate (y, x) (pixels (f (Im seaMonster)))
  where
    translate (dy, dx) = Set.map (\(y, x) -> (y + dy, x + dx))
    ymin = minimum (Set.map fst ps)
    xmin = minimum (Set.map snd ps)
    ymax = maximum (Set.map fst ps)
    xmax = maximum (Set.map snd ps)
    remove haystack needle
        | needle `Set.isSubsetOf` haystack = Set.difference haystack needle
        | otherwise                        = haystack

main = do
    input <- readFile "input.txt"
    let [(images, "")] = readP_to_S (tiles <* skipSpaces <* eof) input
        size = truncate (sqrt (fromIntegral (length images))) - 1

    -- Part 1
    -- Find edge image candidates: images with an edge that does not match
    -- any other edge
    let edgeIds = Set.unions
                $ filter ((== 1) . Set.size)
                $ Map.elems
                $ Map.fromListWith Set.union [(e, Set.singleton n) | (n, im) <- images, f <- transforms, let im' = f im, e <- edges im']
        edgeIms = filter ((`Set.member` edgeIds) . fst) images

    -- Solved the edges using A* (DFS would've been sufficient)
    let Just ((State solved1 unsolved1), _) = find (done1 size . fst) (astar rep (next True size (Map.fromList images)) (heur size) (init edgeIms))
    print $ product $ map fst $ mapMaybe (`Map.lookup` solved1) $ corners size

    -- Part 2
    -- Solved the rest of the image
    let unsolved2 = map fst images \\ map fst (Map.elems solved1)
        state2 = State solved1 unsolved2
        Just ((State solved2 []), _) = find (done2 size . fst) (astar rep (next False size (Map.fromList images)) (heur size) [state2])
        im = assemble solved2

    print $ length $ removeSeaMonsters im
