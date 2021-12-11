import Control.Monad
import Data.Array
import Data.Char

type Octopi = Array (Int, Int) Int

showOctopi a = unlines [[intToDigit (a ! (r, c)) | c <- [c0..c1]] | r <- [r0..r1]]
    where
        ((r0, c0), (r1, c1)) = bounds a

step :: Octopi -> (Int, Octopi)
step a = (n, clamp a'')
    where
        a' = fmap (+ 1) a
        flashing = [p | (p, energy) <- assocs a', energy == 10]
        (n, _, a'') = until done propagate (length flashing, flashing, a')

clamp = fmap (\x -> if x > 9 then 0 else x)

done (n, flashing, a) = null flashing

-- Propagate energy to neighbors of flashing octopi
propagate (n, flashing, a) = (n + length flashing', flashing', a')
    where
        (flashing', a') = foldr f ([], a) (concatMap (neighbors a) flashing)
        f p (fs, a)
            | energy == 10 = (p:fs, a // [(p, energy)])
            | otherwise = (fs, a // [(p, energy)])
            where energy = a ! p + 1

neighbors a (r, c) = do
    dr <- [-1, 0, 1]
    dc <- [-1, 0, 1]
    guard (dr /= 0 || dc /= 0)
    let p = (r + dr, c + dc)
    guard (inRange (bounds a) p)
    return p

main = do
    input <- readFile "input.txt"
    let octopi = listArray ((0, 0), (9, 9)) $ map digitToInt (concat (lines input))
        -- Run simulation and accumulate number of flashes
        steps = iterate (\(n, a) -> let (n', a') = step a in (n + n', a')) (0, octopi)

    -- Part 1
    print $ fst $ steps !! 100

    -- Part 2
    print $ length $ takeWhile (any (/= 0) . snd) steps

