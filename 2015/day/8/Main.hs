process []                = 0
process ('\\':'x':_:_:xs) = 3 + process xs
process ('\\':_:xs)       = 1 + process xs
process ('"':[])          = 1
process ('"':xs)          = 1 + process xs
process (x:xs)            = process xs

encode s = length (show s) - length s

main = do
    input <- readFile "input.txt"

    -- Part 1
    print $ sum $ map process $ lines input

    -- Part 2
    print $ sum $ map encode $ lines input