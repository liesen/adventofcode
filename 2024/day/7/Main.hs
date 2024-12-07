{-# LANGUAGE ViewPatterns #-}

parseLine :: String -> (Int, [Int])
parseLine (break (== ':') -> (ans, ':' : ' ' : terms)) = (read ans, map read (words terms))

eval concat (x : xs) = foldl combine [x] xs
  where
    combine ys x = map (x +) ys ++ map (x *) ys ++ if concat then map (\y -> read (show y ++ show x)) ys else []

main = do
  input <- readFile "input"
  let equations = map parseLine (lines input)

  -- Part 1
  print $ sum [expected | (expected, terms) <- equations, expected `elem` eval False terms]

  -- Part 2
  print $ sum [expected | (expected, terms) <- equations, expected `elem` eval True terms]