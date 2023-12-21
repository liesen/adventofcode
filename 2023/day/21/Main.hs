import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

neighbors g (r, c) =
  [ p'
    | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
      let p' = (r + dr, c + dc),
      p' `Map.member` g
  ]

steps :: Map (Int, Int) [(Int, Int)] -> (Int, Int) -> [Set (Int, Int)]
steps g = iterate step . Set.singleton
  where
    step = foldMap (Set.fromList . neighbors g)

main = do
  input <- readFile "input"
  let grid =
        Map.fromList
          [ ((r, c), ch)
            | (r, ln) <- zip [0 ..] (lines input),
              (c, ch) <- zip [0 ..] ln
          ]
      start = head [p | (p, 'S') <- Map.assocs grid]
      plots = [p | (p, ch) <- Map.assocs grid, ch `elem` ".S"]
      graph = Map.fromList [(p, neighbors graph p) | p <- plots]

  print $ length $ steps graph start !! 64
