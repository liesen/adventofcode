--- Day 19: An Elephant Named Joseph ---
elves n = zip [1..n] (replicate n 1)

f ((n, x):(m, y):zs) xs = f zs (xs ++ [(n, x + y)])
f []                 xs = f xs []
f [z]                [] = z
f [z]                xs = f (z:xs) []

test = f (elves 5) []

-- Looking at the first couple of hundred solutions, it appears that every
-- 2^n - 1 

solution1 =
    let n = until (\n -> 2^n - 1 > 3005290) (+ 1) 1 - 1
        m = 3005290 - (2^n - 1)
    in m * 2 - 1

main1 = print solution1

--- Part Two ---

main2 = print ""


main = main1 >> main2
