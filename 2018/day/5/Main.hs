import Data.Char
import Data.List
import Data.Ord


fix f x = let x' = f x in if x == x' then x else f x'


react []       = (0, [])
react [x]
    | isAlpha x = (0, [x])
    | otherwise = error $ "bad input: '" ++ show (ord x) ++ "'"
react (x:y:ys)
    | isUnit x y = let (n, xs) = react ys in (n + 1, xs)
    | otherwise  = let (n, xs) = react (y:ys) in (n, x:xs)

scan s = snd $ until ((== 0) . fst) (react . snd) (-1, s)

test = length . scan

isUnit x y = x /= y && toUpper x == toUpper y

main = do
    input <- filter isAlpha <$> readFile "input.txt"

    -- Part 1
    print $ test input

    -- Part 2
    let uns = nub (filter isLower input)
    print $ minimum $ map (\x -> test (filter (\y -> x /= y && toUpper x /= y) input)) $ uns
