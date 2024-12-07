import Debug.Trace

safe1 :: [Int] -> Bool
safe1 levels = increasing || decreasing
  where
    diffs = zipWith (-) levels (drop 1 levels)
    increasing = all (\d -> 1 <= d && d <= 3) diffs
    decreasing = all (\d -> -3 <= d && d <= -1) diffs

-- Safe check in a single pass over the input
safe1' :: [Int] -> Bool
safe1' = go (True, True, True)
  where
    go (ok, increasing, decreasing) [x] = ok && (increasing || decreasing)
    go (ok, increasing, decreasing) (x:y:ys) = go (ok && 1 <= d && d <= 3, increasing && y > x, decreasing && y < x) (y:ys)
      where d = abs (x - y)


safe2 :: [Int] -> Bool
safe2 levels = any (\i -> safe1 (take i levels ++ drop (i + 1) levels)) [0 .. length levels - 1]

main = do
  input <- readFile "input"
  let reports = map (map read . words) (lines input)

  -- Part 1
  print $ length $ filter safe1 reports

  -- Part 2
  print $ length $ filter safe2 reports