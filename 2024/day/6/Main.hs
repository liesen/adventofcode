import Data.Set (Set)
import Data.Set qualified as Set

data Walk = Escape (Set (Int, Int)) | Loop deriving (Eq)

walk :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Set (Int, Int) -> Walk
walk ((minrow, mincol), (maxrow, maxcol)) startPos obstacles =
    go (Set.singleton (startPos, startDir)) (startPos, startDir)
  where
    startDir = (-1, 0)
    go visited (pos@(r, c), dir@(dr, dc))
      | not (minrow <= r' && r' <= maxrow && mincol <= c' && c' <= maxcol) = Escape (Set.map fst visited)
      | otherwise =
          let state' = if pos' `Set.member` obstacles
              -- Obstactle ahead: turn
              then (pos, dir')
              -- else step
              else (pos', dir)
          in
            if state' `Set.member` visited 
              then Loop
              else go (Set.insert state' visited) state'
      where
        dir' = (dc, -dr)
        pos'@(r', c') = (r + dr, c + dc)

main = do
  input <- readFile "input"
  let rows = (lines input)
      numrows = length rows
      numcols = length (rows !! 0)
      bounds = ((0, 0), (numrows - 1, numcols - 1))
      grid = [((r, c), ch) | (r, ln) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] ln]
  let [startPos] = [p | (p, '^') <- grid]
      obstacles = Set.fromList [p | (p, '#') <- grid]
      
  -- Part 1
  let Escape path = walk bounds startPos obstacles
  print $ length path

  -- Part 2
  let loops x = walk bounds startPos (Set.insert x obstacles) == Loop
  print $ length $ filter loops $ Set.toList path