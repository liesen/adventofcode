import Data.List

main = do
  input <- readFile "input"
  let [ls, rs] = transpose $ map (map read . words) $ lines input

  -- Part 1
  print $ sum $ map abs $ zipWith (-) (sort ls) (sort rs)

  -- Part 2
  print $ sum $ map (\l -> l * length (filter (== l) rs)) ls