import Data.Array (range)
import Data.List
import Data.Ord
import Data.Char
import Data.Function


parseLine :: String -> (Int, Int)
parseLine s = read $ "(" ++ s ++ ")"

parse = map parseLine . lines

dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

main = do
    input <- readFile "input.txt"
    let coords = parse input

    -- Find the boundaries of the grid
    let x0 = minimum $ map fst coords
        x1 = maximum $ map fst coords
        y0 = minimum $ map snd coords
        y1 = maximum $ map snd coords
        bounds = ((x0, y0), (x1, y1))

    -- Part 1
    -- Find how many points are closest to each of the target coordinates
    let nearest = [ q
                  | p <- range bounds
                    -- Find distances to p for each target coordinate
                  , let dists = map (\q -> (dist p q, q)) coords
                    -- Filter ties in distance
                  , [(_, q)] <- take 1 $ groupBy ((==) `on` fst) $ sortBy (comparing fst) dists
                  ]
    let nearestCount = map (\xs -> (head xs, length xs)) $ group $ sort nearest

    -- Filter points that have neighboring points that span a rectangle
    let finites = do
        p@(x0, y0) <- coords
        ne <- take 1 [q | q@(x1, y1) <- coords, q /= p, x1 > x0 && y1 < y0]
        nw <- take 1 [q | q@(x2, y2) <- coords, q /= p, x2 < x0 && y2 < y0]
        se <- take 1 [q | q@(x3, y3) <- coords, q /= p, x3 > x0 && y3 > y0]
        sw <- take 1 [q | q@(x4, y4) <- coords, q /= p, x4 < x0 && y4 > y0]
        return p

    case maximum (map (flip lookup nearestCount) finites) of
        Just n -> print n
        Nothing -> print "Fatal chronal interference detected. Aborting."

    -- Part 2
    let region = [p | p <- range bounds, sum (map (dist p) coords) < 10000]
        size = length region
    print size
