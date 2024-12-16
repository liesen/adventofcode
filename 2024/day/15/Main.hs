{-# LANGUAGE RecordWildCards, ViewPatterns #-}
import Data.Maybe
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
            | (pos, '[') `elem` boxes = '['
            | (pos, ']') `elem` boxes = ']'
            | (pos, 'O') `elem` boxes = 'O'
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

moveM walls ((robot@(r, c)), boxes) (dr, dc)
  | robot' `elem` walls = fail "walls"
  | Just 'O' <- box = do
    (r, bs) <- moveM walls (robot', boxes) (dr, dc)
    return (robot', Set.insert (r, 'O') (Set.delete (robot', 'O') bs))
  | dr == 0, Just b <- box = do
    (r, bs) <- moveM walls (robot', boxes) (dr, dc)
    return (robot', Set.insert (r, b) (Set.delete (robot', b) bs))
  | Just b1 <- box =
    let (b2, dc') = if b1 == '[' then (']', 1) else ('[', -1)
        robot'' = (r + dr, c + dc')
    in do
      (r1, bs1) <- moveM walls (robot', boxes) (dr, dc)
      (r2, bs2) <- moveM walls (robot'', bs1) (dr, dc)
      return (robot',
        Set.insert (r2, b2) (Set.delete (robot'', b2) (Set.insert (r1, b1) (Set.delete (robot', b1) bs2)))
       )
  | otherwise = return (robot', boxes)
  where
    robot' = (r + dr, c + dc)
    box
      | (robot', 'O') `elem` boxes = Just 'O'
      | (robot', '[') `elem` boxes = Just '['
      | (robot', ']') `elem` boxes = Just ']'
      | otherwise = Nothing

moveM' walls s d =
  case moveM walls s d of
    Nothing -> s
    Just s' -> s'

sumGps (Warehouse {..}) = 
    sum [100 * r + c | ((r, c), 'O') <- Set.toList boxes ]
    + sum [100 * r + c | ((r, c), '[') <- Set.toList boxes ]

scale (w@(Warehouse {..})) = w { numcols = numcols', walls = walls', boxes = boxes', robot = robot' }
  where
    numcols' = numcols * 2
    walls' = Set.fromList [(r, 2 * c + dc) | (r, c) <- Set.toList walls, dc <- [0, 1]]
    boxes' = Set.fromList [((r, 2 * c + dc), ch) | ((r, c), 'O') <- Set.toList boxes, (dc, ch) <- [(0, '['), (1, ']')]]
    robot' = let (r, c) = robot in (r, 2 * c)
    

main = do
    input <- readFile "input"
    let (w1, ms) = parse input
    
    -- Part 1
    let (r1, b1) = foldl (moveM' (walls w1)) (robot w1, boxes w1) (map d ms)
    print $ sumGps $ w1 { robot = r1, boxes = b1 }
    
    -- Part 2
    let w2 = scale w1
        (r2, b2) = foldl (moveM' (walls w2)) (robot w2, boxes w2) (map d ms)
    print $ sumGps $ w2 { robot = r2, boxes = b2 }