{-# LANGUAGE RecordWildCards, ViewPatterns #-}
import Control.Arrow ((&&&))
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import Text.ParserCombinators.ReadP


data Team = ImmuneSystem
          | Infection
    deriving (Eq, Ord, Show)

opponent ImmuneSystem = Infection
opponent Infection = ImmuneSystem

opponents g = filter (\g' -> team g /= team g')

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
    } deriving (Eq)

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
effectivePower Group{..} = numberOfUnits * attackDamage

showGroup :: Group -> String
showGroup (g@Group {..}) = "Group " ++ show index ++ " contains " ++ show numberOfUnits ++ " units (effective power: " ++ show (effectivePower g) ++ ")"

data TargetSelection = TargetSelection
    { attacker :: Group
    , defender :: Group
    }

instance Show TargetSelection where
    show TargetSelection {..} =
        showTeam attackingTeam ++ " group " ++ show attackingTeamIndex ++ " would deal defending group " ++ show defendingTeamIndex ++ " " ++ show (effectiveness attacker defender) ++ " damage"
      where
        (attackingTeam, attackingTeamIndex) = (team attacker, index attacker)
        (defendingTeam, defendingTeamIndex) = (team defender, index defender)
        showTeam ImmuneSystem = "Immune system"
        showTeam Infection = "Infection"

-- targetSelection :: GameState -> _
targetSelection (Groups groups) = foldl select mempty targets
  where
    targets = do
        attacker <- sortOn (Down . effectivePower &&& Down . initiative) groups
        defender <- sortOn (Down . effectiveness attacker &&& Down . effectivePower) (opponents attacker)
        return TargetSelection {..}

    opponents attacker = [g | g <- groups, team g /= team attacker]

    select xs x
        | attacker x `elem` map attacker xs = xs
        | defender x `elem` map defender xs = xs
        | otherwise = x:xs

attacking :: Groups -> [TargetSelection] -> [Group]
attacking (Groups groups) (foldl attack groups . sortOn (Down . initiative . attacker) -> groups') = nubBy ((==) `on` (team &&& index)) groups'

alive Group {..} = numberOfUnits > 0

attack groups (TargetSelection {..}) = -- traceShow ((TargetSelection attacker' defender'), kills) $
    defender'{numberOfUnits = n - kills}:groups
  where
    Just attacker' = find (\g -> team g == team attacker && index g == index attacker) groups
    Just defender' = find (\g -> team g == team defender && index g == index defender) groups
    n = numberOfUnits defender'
    kills = effectiveness attacker' defender' `div` hitPoints defender'

step :: Groups -> Groups
step g0@(Groups {..}) = Groups $ filter alive (groups' ++ [g | g <- groups, (team g, index g) `notElem` map (\h -> (team h, index h)) groups'])
  where
    groups' = attacking g0 (targetSelection g0)

done (Groups {..})
    | null immuneSystem && not (null infection) = Just Infection
    | not (null immuneSystem) && null infection = Just ImmuneSystem
    | otherwise = Nothing
  where
    immuneSystem = filter ((== ImmuneSystem) . team) groups
    infection = filter ((== Infection) . team) groups

effectiveness attacker@(Group {attackType = attackType}) defender@(Group {immunities = immunities, weaknesses = weaknesses})
    | attackType `elem` immunities = 0
    | attackType `elem` weaknesses = 2 * effectivePower attacker
    | otherwise                    = effectivePower attacker

main = do
    input <- readFile "input.txt"
    let [(g, "")] = readP_to_S (parse <* eof) input
        Groups groups = g

    let Groups groups' = until (isJust . done) step g
    print $ sum $ map numberOfUnits groups'
