import Data.List
import Data.Ord

data Item = Item
    { itemName :: String
    , itemCost :: Int
    , itemDamage :: Int
    , itemArmor :: Int
    } deriving (Eq, Show)

-- This type gets overloaded for both item configuration and player entities, yuck!
data Items = Items
    { identity :: String
    , hpOrCost :: Int
    , damage :: Int
    , armor :: Int
    } deriving (Eq, Show)

-- Generate the combinations of K distinct objects chosen from the N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

-- Generate all possible configurations of weapons, armor and rings
configurations = do
    w <- weapons
    a <- Item "No armor" 0 0 0 : armors
    r <- [mempty] ++ map (:[]) rings ++ combinations 2 rings
    let items = w:a:r
        identity = intercalate ", " (map itemName items)
        cost = sum (map itemCost items)
        damage = sum (map itemDamage items)
        armor = sum (map itemArmor items)
    return $ Items identity cost damage armor
  where
    weapons = [ Item "Dagger" 8 4 0
              , Item "Shortsword" 10 5 0
              , Item "Warhammer" 25 6 0
              , Item "Longsword" 40 7 0
              , Item "Greataxe" 74 8 0
              ]
    armors = [ Item "Leather" 13 0 1
             , Item "Chainmail" 31 0 2
             , Item "Splitmail" 53 0 3
             , Item "Bandedmail" 75 0 4
             , Item "Platemail" 102 0 5
             ]
    rings = [ Item "Damage +1" 25 1 0
            , Item "Damage +2" 50 2 0
            , Item "Damage +3" 100 3 0
            , Item "Defense +1" 20 0 1
            , Item "Defense +2" 40 0 2
            , Item "Defense +3" 80 0 3
            ]

dealDamage :: Items -> Items -> Items
dealDamage attacker defender =
    let dmg = max 1 (damage attacker - armor defender)
    in defender { hpOrCost = hpOrCost defender - dmg }

play :: Items -> Items -> [Items]
play attacker defender = let p = dealDamage attacker defender in p : play p attacker

newPlayer items = items { identity = "Player", hpOrCost = 100 }

playerWins :: Items -> Items -> Bool
playerWins boss player =
    let loser = head $ dropWhile ((> 0) . hpOrCost) $ play player boss
    in identity boss == identity loser

main = do
    let boss = Items "Boss" 109 8 2

    -- Part 1
    print $ minimum $ map hpOrCost $ filter (playerWins boss . newPlayer) configurations

    -- Part 2
    print $ maximum $ map hpOrCost $ filter (not . playerWins boss . newPlayer) configurations

