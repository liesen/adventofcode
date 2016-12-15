--- Day 9: Explosives in Cyberspace ---

decompress []       = []
decompress (' ':xs) = decompress xs
decompress ('(':xs) =
    let (m, 'x':xs') = break (== 'x') xs
        (n, ')':xs'') = break (== ')') xs'
        (ys, ys') = splitAt (read m) xs''
    in concat (replicate (read n) ys) ++ decompress ys'
decompress (x:xs)   = x : decompress xs

decompressLen []       = 0
decompressLen (' ':xs) = decompressLen xs
decompressLen ('(':xs) =
    let (m, 'x':xs') = break (== 'x') xs
        (n, ')':xs'') = break (== ')') xs'
        (_, xs''') = splitAt (read m) xs''
    in read m * read n + decompressLen xs'''
decompressLen (x:xs)   = 1 + decompressLen xs

main1 = readFile "input.txt" >>= print . pred . length . decompress

--- Part Two ---
decompress2 []       = []
decompress2 (' ':xs) = decompress2 xs
decompress2 ('(':xs) =
    let (m, 'x':xs') = break (== 'x') xs
        (n, ')':xs'') = break (== ')') xs'
        (ys, ys') = splitAt (read m) xs''
    in concat (replicate (read n) (decompress2 ys)) ++ decompress2 ys'
decompress2 (x:xs)   = x : decompress2 xs

decompressLen2 []       = 0
decompressLen2 (' ':xs) = decompressLen2 xs
decompressLen2 ('(':xs) =
    let (m, 'x':xs') = break (== 'x') xs
        (n, ')':xs'') = break (== ')') xs'
        (ys, ys') = splitAt (read m) xs''
    in read n * decompressLen2 ys + decompressLen2 ys'
decompressLen2 (x:xs)   = 1 + decompressLen2 xs

-- For some reason decompressLen{2} gives the length + 1
main = readFile "input.txt" >>= \input -> do
    print (decompressLen input - 1)
    print (decompressLen2 input - 1)
