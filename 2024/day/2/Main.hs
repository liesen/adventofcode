safe1 :: [Int] -> Bool
safe1 levels = increasing || decreasing
  where
    diffs = zipWith (-) levels (drop 1 levels)
    increasing = all (\d -> 1 <= d && d <= 3) diffs 
    decreasing = all (\d -> -3 <= d && d <= -1) diffs
    
safe2 :: [Int] -> Bool
safe2 levels = any (\i -> safe1 (take i levels ++ drop (i + 1) levels)) [0..length levels - 1]

main = do
    input <- readFile "input"
    let reports = map (map read . words) (lines input)
    
    -- Part 1
    print $ length [report | report <- reports, safe1 report]
    
    -- Part 2
    print $ length [report | report <- reports, safe2 report]