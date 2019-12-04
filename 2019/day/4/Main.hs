import Data.List

increasing len = go len 1
  where
    go 0   _ = [[]]
    go len d = do
        x <- [d..9]
        xs <- go (len - 1) x
        return (x:xs)

main = do
    let input = "146810-612564"
        range@(lo, hi) = ([1,4,6,8,1,0], [6,1,2,5,6,4])

    -- Part 1
    print $ length
          $ filter (any ((>= 2) . length) . group)
          $ takeWhile (<= hi)
          $ dropWhile (<= lo)
          $ increasing 6

    -- Part 2
    print $ length
          $ filter (any ((== 2) . length) . group)
          $ takeWhile (<= hi)
          $ dropWhile (<= lo)
          $ increasing 6