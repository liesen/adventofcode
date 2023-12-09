{-# LANGUAGE ViewPatterns #-}

extrapolate xs
  | all (== 0) xs = (0, 0)
  | otherwise = let (front, back) = extrapolate (zipWith (-) (tail xs) xs) in (head xs - front, last xs + back)

main = do
  input <- map (map read . words) . lines <$> readFile "input"
  let (sum -> ans2, sum -> ans1) = unzip $ map extrapolate input
  print ans1
  print ans2