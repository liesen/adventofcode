step :: Int -> Int
step code = snd $ (code * 252533) `divMod` 33554393

main = do
    let row = 2981
        col = 3075
    
    -- Find index of item at (row, col)
    let index =
        let r = row - 1  -- Adjust for 1-based indices
            c = col - 1
        in (r * (r + 1)) `div` 2 + 1  -- Start value of row r
           + (sum (take c [r + 2..]) - 1)

    print $ iterate step 20151125 !! i
    