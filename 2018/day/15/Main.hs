{-# LANGUAGE TupleSections, ViewPatterns #-}
module Main where

import Control.Monad.Except
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (Semigroup((<>)))
import Data.Function (on)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Ord (comparing)


type Pos = (Int, Int)

-- adjacent (immediately up, down, left, or right)
adjacent :: Pos -> [Pos]
adjacent (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

data Unit = Unit
    { unitId :: Int
    , position :: Pos
    , team :: Char
    , attackPower :: Int
    , hp :: Int
    }
    deriving (Show)

alive :: Unit -> Bool
alive = (> 0) . hp

elf :: Unit -> Bool
elf = (== 'E') . team

data World = World
    { open :: Set Pos
    , size :: (Int, Int)
    , units :: [Unit]
    }

instance Show World where
    show (World a size units) =
        unlines [row r | r <- [0..maxrow + 1]]
      where
        (maxrow, maxcol) = size
        unitHp unit = show (team unit) ++ "(" ++ show (hp unit) ++ ")"

        row r = ln ++ "  " ++ intercalate "," (map unitHp rowUnits)
          where
            (ln, catMaybes -> rowUnits) = mapAccumL p "" [0..maxcol + 1]
            p ln c = case find (\u -> position u == (r, c) && alive u) units of
                         Nothing -> if (r, c) `elem` a then (ln ++ ".", Nothing) else (ln ++ "#", Nothing)
                         Just unit -> (ln ++ show (team unit), Just unit)

instance Semigroup World where
    World o1 (r1, c1) u1 <> World o2 (r2, c2) u2 = World (o1 <> o2) (r1 `max` r2, c1 `max` c2) (u1 `mergeUnits` u2)
      where
        -- Fixup unitIds
        mergeUnits u1 u2 = zipWith (\unitId unit -> unit { unitId = unitId }) [1..length (u1 <> u2)] (u1 <> u2)

instance Monoid World where
    mempty = World mempty (0, 0) mempty

-- parse :: String -> World
parse :: String -> World
parse s =
    foldMap f $
    [ ((r, c), x)
    | (r, ln) <- zip [0..] (lines s)
    , (c, x) <- zip [0..] ln
    ]
  where
    f (p, '.') = World (Set.singleton p) p mempty
    f (p, 'E') = World (Set.singleton p) p [Unit 0 p 'E' 3 200]
    f (p, 'G') = World (Set.singleton p) p [Unit 0 p 'G' 3 200]
    f (p, '#') = mempty
    f _        = error "no parse"


step :: World -> Either World World
step world = foldM stepUnit world $ sortOn position (units world)

stepUnit :: World -> Unit -> Either World World
stepUnit world unit0
    | not (alive unit) =
        -- Unit is dead: end turn
        Right world
    | null targets =
        -- No target squares to move to: end **round**
        Left world
    | position unit `notElem` inRange =
        -- Unit not in range for an attack: move, then attack!
        case findMove world unit inRange of
            Nothing -> Right (attack world unit)
            Just pos' ->
                let (unit', world') = move unit pos'
                in Right (attack world' unit')
    | otherwise =
        -- Unit is in range to attack an enemy: attack!
        Right (attack world unit)
  where
    -- Look up the unit in the most recent world, it might be dead D:
    Just unit = find ((== unitId unit0) . unitId) (units world)

    targets = [ target
              | target <- units world
              , team unit /= team target
              , alive target
              ]

    occupied = Set.fromList $
        [ position u
        | u <- units world
        , position unit /= position u
        , alive u
        ]

    inRange = Set.fromList $
        [ pos
        | target <- targets
        , pos <- adjacent (position target)
        , pos `elem` open world
        , pos `notElem` occupied
        ]

    move unit pos' = (unit', world')
      where
        unit' = unit { position = pos' }
        world' = world { units = [if unitId u == unitId unit then unit' else u | u <- units world]}

    attack world attacker =
        case sortOn (\defender -> (hp defender, position defender)) opponents of
            [] -> world
            defender:_ ->
                world {units = [if unitId defender == unitId u then defender {hp = hp defender - attackPower unit} else u | u <- units world]}
      where
        opponents =
            [ target
            | target <- targets
            , position target `elem` adjacent (position attacker)
            ]

findMove :: World -> Unit -> Set Pos -> Maybe Pos
findMove world unit targets =
    case candidates of
        [] -> Nothing
        nearest:_ ->
            let chosen = minimumBy (comparing fst) nearest
            in Just (backtrack chosen)
  where
    pos = position unit

    distances :: Map Pos (Int, Maybe Pos)
    distances = Map.fromList (bfs world pos)

    dist (pos, (dist, prev)) = dist

    candidates =
        groupBy ((==) `on` dist) $
        sortOn dist $
        mapMaybe (\target -> fmap (target,) (Map.lookup target distances)) (Set.toList targets)

    backtrack :: (Pos, (Int, Maybe Pos)) -> Pos
    backtrack (pos, (dist, Just pos')) =
        case Map.lookup pos' distances of
            Just (dist', Nothing) -> pos
            Just (dist', Just pos'') -> backtrack (pos', (dist, Just pos''))

bfs world start = go [(start, (0, Nothing))] mempty
  where
    occupied :: Set Pos
    occupied = Set.fromList [position unit | unit <- units world, alive unit]

    go :: [(Pos, (Int, Maybe Pos))] -> Set Pos -> [(Pos, (Int, Maybe Pos))]
    go []                  seen = []
    go ((pos, (dist, prev)):queue) seen
        | pos `notElem` open world = go queue seen
        | pos `elem` occupied && pos /= start = go queue seen
        | pos `elem` seen = go queue seen
        | otherwise = (pos, (dist, prev)) : go (queue Data.Semigroup.<> next (pos, (dist, prev))) (Set.insert pos seen)

    next :: (Pos, (Int, Maybe Pos)) -> [(Pos, (Int, Maybe Pos))]
    next (pos, (dist, prev)) = [(pos', (dist + 1, Just pos)) | pos' <- sort (adjacent pos)]

gameOver world = null elves || null goblins
  where
    (elves, goblins) = partition elf $ filter alive $ units world

sumHp = sum . map hp . filter alive . units

run = go 0
  where
    go round world
      | gameOver world = round * sumHp world
      | otherwise = case step world of
                        Left world' -> round * sumHp world'
                        Right world' -> go (round + 1) world'

main = do
    input <- readFile "input.txt"
    let world = parse input

    -- Part 1
    print $ run world