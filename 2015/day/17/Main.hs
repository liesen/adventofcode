pack 0 _      = 1
pack n []     = 0
pack n (x:xs) = pack n xs + if x <= n then pack (n - x) xs else 0


main = do
    input <- readFile "input.txt"
    let containers = map read (lines input)

    print $ pack 150 containers