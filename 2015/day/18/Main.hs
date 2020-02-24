import Data.Array (range)
import qualified Data.Map as Map
import Data.Map (Map, (!))

data Grid = Grid !Int !Int (Map (Int, Int) Int)

unGrid (Grid _ _ m) = m

instance Show Grid where
    show (Grid maxrow maxcol m) =
        unlines [ [ char (m ! (r, c)) | c <- [0..maxcol - 1] ]
                  | r <- [0..maxrow - 1] ]
      where char 1 = '#'
            char 0 = '.'

parse :: String -> Grid
parse s = Grid maxrow maxcol $ Map.fromList [(pos, if ch == '#' then 1 else 0) | (pos, ch) <- zip (range ((0, 0), (maxrow - 1, maxcol - 1))) (concat ys)]
  where
    ys = lines s
    maxrow = length ys
    maxcol = length (head ys)

neighbors (r, c) = [ (r - 1, c - 1)
                   , (r - 1, c)
                   , (r - 1, c + 1)
                   , (r, c - 1)
                   , (r, c + 1)
                   , (r + 1, c - 1)
                   , (r + 1, c)
                   , (r + 1, c + 1) ]

step (Grid maxrow maxcol m) = Grid maxrow maxcol $ Map.fromList $ map f $ Map.assocs m
  where
    f (pos, 1)
        | elem (neighborsOn pos) [2, 3] = (pos, 1)
        | otherwise = (pos, 0)
    f (pos, 0)
        | neighborsOn pos == 3 = (pos, 1)
        | otherwise = (pos, 0)
    
    neighborsOn = sum . map (maybe 0 id . (`Map.lookup` m)) . neighbors
    
main = do
    input <- readFile "input.txt"
    let a = parse input

    -- Part 1
    print $ sum . Map.elems . unGrid $ iterate step a !! 100