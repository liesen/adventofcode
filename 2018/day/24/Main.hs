{-# LANGUAGE RecordWildCards #-}
import Control.Arrow ((&&&))
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.ReadP


data Team = ImmuneSystem
          | Infection
    deriving (Eq, Ord)

instance Show Team where
    show ImmuneSystem = "Immune System"
    show Infection = "Infection"

opponent ImmuneSystem = Infection
opponent Infection = ImmuneSystem

data Group = Group
    { team :: Team
    , index :: Int
    , numberOfUnits :: Int
    , hitPoints :: Int
    , attackDamage :: Int
    , attackType :: String
    , initiative :: Int
    , weaknesses :: [String]
    , immunities :: [String]
    }

groupId = team &&& index

instance Eq Group where
    (==) = (==) `on` groupId

instance Show Group where
    show Group {..} =
        show numberOfUnits ++ " units " ++
        "each with " ++ show hitPoints ++ " hit points" ++
        immunitiesAndWeaknesses ++ " with an attack that does " ++ show attackDamage ++ " " ++ attackType ++ " damage" ++
        " at initiative " ++ show initiative
      where
        immunitiesAndWeaknesses
            | null immunities && null weaknesses = ""
            | not (null immunities) && null weaknesses = " (immune to " ++ intercalate ", " immunities ++ ")"
            | null immunities && not (null weaknesses) = " (weak to " ++ intercalate ", " weaknesses ++ ")"
            | otherwise = " (immune to " ++ intercalate ", " immunities ++ "; weak to " ++ intercalate ", " weaknesses ++ ")"

effectivePower :: Group -> Int
effectivePower Group {..} = attackDamage * max 0 numberOfUnits

alive Group {..} = numberOfUnits > 0

newtype Groups = Groups { groups :: [Group] }

instance Show Groups where
    show (Groups gs) = unlines $
        ["Immune System:"] ++
        [show g | g <- sortOn index gs, team g == ImmuneSystem] ++
        ["", "Infection:"] ++
        [show g | g <- sortOn index gs, team g == Infection]

parse :: ReadP Groups
parse = do
    _ <- string "Immune System:\n"
    immuneSystem <- endBy1 (parseGroup ImmuneSystem) (char '\n')
    _ <- char '\n'
    _ <- string "Infection:\n"
    infection <- endBy1 (parseGroup Infection) (char '\n')
    let groups = zipWith (\i g -> g {index = i}) [1..] immuneSystem ++ zipWith (\i g -> g {index = i}) [1..] infection
    return Groups {..}

num :: ReadP Int
num = fmap read (many1 (satisfy isDigit))

parseGroup :: Team -> ReadP Group
parseGroup team = do
    numberOfUnits <- num <* string " units"
    _ <- string " each with "
    hitPoints <- num <* string " hit points"
    (immunities, weaknesses) <- option ([], []) (between (string " (") (char ')') parseImmunitiesAndWeaknesses)
    _ <- string " with an attack that does "
    attackDamage <- num
    _ <- char ' '
    attackType <- many (satisfy isAlpha)
    _ <- string " damage at initiative "
    initiative <- num
    return Group {..}
  where
    index = 0

parseImmunitiesAndWeaknesses :: ReadP ([String], [String])
parseImmunitiesAndWeaknesses =
    ((,) <$> option [] immunities <*> option [] (string "; " *> weaknesses))
    +++
    (flip (,) <$> option [] weaknesses <*> option [] (string "; " *> immunities))
  where
    immunities = string "immune to " *> sepBy1 (many (satisfy isAlpha)) (string ", ")
    weaknesses = string "weak to " *> sepBy1 (many (satisfy isAlpha)) (string ", ")

data TargetSelection = TargetSelection
    { attacker :: Group
    , defender :: Group
    }

instance Show TargetSelection where
    show TargetSelection {..} =
        show (team attacker) ++ " group " ++ show (index attacker) ++
        " would deal defending group " ++ show (team defender) ++ " " ++
        show (effectiveDamage attacker defender) ++ " damage"

targetSelection :: Groups -> [TargetSelection]
targetSelection (Groups gs) = selection
  where
    targets = do
        attacker <- sortOn (effectivePower &&& initiative) gs
        defender <- sortOn (effectiveDamage attacker &&& effectivePower &&& initiative) (opponents attacker)
        guard (attackType attacker `notElem` immunities defender)
        return TargetSelection {..}

    opponents g = filter (\g' -> team g /= team g') gs

    selection = foldr select mempty targets  -- Make sure attackers and defenders are chosen only once

    select x xs
        | attacker x `elem` map attacker xs = xs  -- Attacker already chosen
        | defender x `elem` map defender xs = xs  -- Defender already chosen
        | otherwise = x:xs

damageMultiplier :: Group -> Group -> Int
damageMultiplier attacker defender
    | attackType attacker `elem` immunities defender = 0
    | attackType attacker `elem` weaknesses defender = 2
    | otherwise                                      = 1
  
effectiveDamage attacker defender = effectivePower attacker * damageMultiplier attacker defender

attacking :: Groups -> [TargetSelection] -> Groups
attacking (Groups gs) sel =
    let gs' = foldr attack gs $ sortOn (initiative . attacker) sel
    in Groups $ nub (gs' ++ gs)

attack (TargetSelection {..}) gs = defender'{numberOfUnits = n - kills}:gs
  where
    Just attacker' = find (== attacker) gs -- Find "updated" attacker
    Just defender' = find (== defender) gs
    n = numberOfUnits defender'
    kills = effectiveDamage attacker' defender' `div` hitPoints defender'

step :: Groups -> Groups
step g = Groups $ filter alive gs'
  where
    Groups gs' = attacking g (targetSelection g)

winner (Groups gs)
    | null immuneSystem && not (null infection) = Just Infection
    | not (null immuneSystem) && null infection = Just ImmuneSystem
    | otherwise = Nothing
  where
    immuneSystem = filter ((== ImmuneSystem) . team) gs
    infection = filter ((== Infection) . team) gs

countUnits (Groups gs) = sum (map numberOfUnits gs)

boost (Groups gs) i = Groups (map f gs)
  where
    f g@(Group {..}) | team == ImmuneSystem = g {attackDamage = attackDamage + i}
                     | otherwise = g

main = do
    input <- readFile "input.txt"
    let [(g0, "")] = readP_to_S (parse <* eof) input

    -- Part 1
    let ans1 = until (isJust . winner) step g0
    print $ countUnits ans1

    -- Part 2
    -- Boost number found by trial and error.  41 is a win for the infection,
    -- 42 is a tie (no team deals enough damage to reduce the strength of its
    -- opponent), and 43 is a win for the immune system.
    let ans2 = until (isJust . winner) step (boost g0 43)
    print $ countUnits ans2
