{-# LANGUAGE RecordWildCards, ViewPatterns #-}
import Data.List
import Data.Set qualified as Set
import Data.Set (Set)

type Pos = (Int, Int)

data Warehouse = Warehouse { numrows :: Int, numcols :: Int, walls :: Set Pos, boxes :: Set (Pos, Char), robot :: Pos }

instance Show Warehouse where
  show (Warehouse {..}) = unlines [ [char (r, c) | c <- [0..numcols - 1]] | r <- [0..numrows - 1] ]
    where
        char pos
            | pos == robot = '@'
            | pos `elem` walls = '#'
            | (pos, 'O') `elem` boxes = 'O'
            | (pos, '[') `elem` boxes = '['
            | (pos, ']') `elem` boxes = ']'
            | otherwise = '.'

parse s = (Warehouse {..}, movements)
    where 
        (xs, "":ys) = break (== "") (lines s)
        numrows = length xs
        numcols = length (xs !! 0)
        walls = Set.fromList [(r, c) | (r, row) <- zip [0..] xs, (c, '#') <- zip [0..] row]
        boxes = Set.fromList [((r, c), 'O') | (r, row) <- zip [0..] xs, (c, 'O') <- zip [0..] row]
        robot = head [(r, c) | (r, row) <- zip [0..] xs, (c, '@') <- zip [0..] row]
        movements = concat ys
        
d '^' = (-1, 0)
d 'v' = (1, 0)
d '<' = (0, -1)
d '>' = (0, 1)

move (w@Warehouse{..}) (dr, dc)
  | (r + dr, c + dc) `elem` walls = w { robot = (r, c)} -- fail "walls"
  | ((r + dr, c +dc), 'O') `elem` boxes =
    let w'@(Warehouse { boxes = boxes', robot = robot' }) = move (w { robot = (r+dr, c+dc)}) (dr, dc)
    in if robot' == (r + dr, c + dc)
      then w
      else w' { boxes = Set.insert (robot', 'O') (Set.delete ((r + dr, c + dc), 'O') boxes'), robot = (r + dr, c + dc)}
  | otherwise = w {robot = (r + dr, c + dc)}
  where
    (r, c) = robot
    
sumGps (Warehouse {..}) = sum [100 * r + c | ((r, c), 'O') <- Set.toList boxes ]

scale (w@(Warehouse {..})) = w { numcols = numcols', walls = walls', boxes = boxes', robot = robot' }
  where
    numcols' = numcols * 2
    walls' = Set.fromList [(r, 2 * c + dc) | (r, c) <- Set.toList walls, dc <- [0, 1]]
    boxes' = Set.fromList [((r, 2 * c + dc), ch) | ((r, c), 'O') <- Set.toList boxes, (dc, ch) <- [(0, '['), (1, ']')]]
    robot' = let (r, c) = robot in (r, 2 * c)
    

main = do
    input <- readFile "example3"
    print input
    print $ break (== "") $ lines input
    let (w, ms) = parse input
    -- print ms
    -- let xs = scanl move w (map d ms)
    -- mapM_ print xs
    -- print $ sumGps $ foldl' move w (map d ms)
    print w
    print $ scale w