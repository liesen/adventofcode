{-# LANGUAGE ViewPatterns #-}

parseLine :: String -> (Int, [Int])
parseLine (break (== ':') -> (ans, ':' : ' ' : terms)) = (read ans, map read (words terms))

valid part2 target [x] = target == x
valid part2 target (x : xs) | target < x = False -- Prune continuations without a future (value of expression is strictly increasing)
valid part2 target (x : y : ys) = any (\op -> valid part2 target (op x y : ys)) ops
  where
    ops = [(+), (*)] ++ if part2 then [cat] else []
    cat x y = read (show x ++ show y)

main = do
  input <- readFile "input"
  let equations = map parseLine (lines input)

  -- Part 1
  print $ sum [expected | (expected, terms) <- equations, valid False expected terms]

  -- Part 2
  print $ sum [expected | (expected, terms) <- equations, valid True expected terms]