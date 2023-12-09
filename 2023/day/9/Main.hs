f1 xs
  | all (== 0) xs = 0
  | otherwise = last xs + f1 (zipWith (-) (tail xs) xs)

f2 xs
  | all (== 0) xs = 0
  | otherwise = head xs - f2 (zipWith (-) (tail xs) xs)

main = do
  input <- map (map read . words) . lines <$> readFile "input"
  print $ sum $ map f1 input
  print $ sum $ map f2 input