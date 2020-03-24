import Data.Monoid

--- Day 16: Dragon Checksum ---
input = "10010000000110000"

flip' '1' = '0'
flip' '0' = '1'

checksum []       = []
checksum (x:y:ys) = (if x == y then '1' else '0'):checksum ys

dragon n = fold . unfold
  where
    unfold = until ((>= n) . length) go
      where go a = let b = map flip' (reverse a) in a <> "0" <> b

    fold = until (odd . length) checksum . take n

main = do
  -- Part 1
  putStrLn $ dragon 272 input

  -- Part 2
  putStrLn $ dragon 35651584 input