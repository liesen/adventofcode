{-# LANGUAGE ViewPatterns #-}
step1 (hpos, vpos) (words -> ["forward", read -> n]) = (hpos + n, vpos)
step1 (hpos, vpos) (words -> ["up", read -> n]) = (hpos, vpos - n)
step1 (hpos, vpos) (words -> ["down", read -> n]) = (hpos, vpos + n)

step2 (hpos, vpos, aim) (words -> ["forward", read -> n]) = (hpos + n, vpos + aim * n, aim)
step2 (hpos, vpos, aim) (words -> ["up", read -> n]) = (hpos, vpos, aim - n)
step2 (hpos, vpos, aim) (words -> ["down", read -> n]) = (hpos, vpos, aim + n)

main = do
    input <- readFile "input.txt"

    -- Part 1
    let (hpos, vpos) = foldl step1 (0, 0) (lines input)
    print (hpos * vpos)

    -- Part 2
    let (hpos, vpos, aim) = foldl step2 (0, 0, 0) (lines input)
    print (hpos * vpos)

