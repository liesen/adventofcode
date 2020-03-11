{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}
import Control.Monad
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

data Spell
    = MagicMissile
    | Drain
    | Shield
    | Poison
    | Recharge
    deriving (Bounded, Enum, Eq, Ord, Show)

type Effect = (Int, Spell)

data Player = Player
    { playerHp :: Int
    , playerArmor :: Int
    , playerMana :: Int
    , playerStrategy :: GameState -> [Spell]
    }

instance Show Player where
    show Player{..} = "Player has " ++ show playerHp ++ " hit points, " ++ show playerArmor ++ " armor, " ++ show playerMana ++ " mana"

data Boss = Boss
    { bossHp :: Int
    , bossDamage :: Int
    } deriving (Eq, Ord)

instance Show Boss where
    show Boss{..} = "Boss has " ++ show bossHp ++ " hit points"

data GameState = GameState
    { player :: Player
    , playerManaSpent :: Int
    , boss :: Boss
    , turn :: Int
    , effects :: [Effect]
    }
  deriving (Show)

data Turn
    = PlayerTurn GameState
    | BossTurn GameState
    | GameOver GameState
  deriving (Show)

gameState (PlayerTurn s) = s
gameState (BossTurn s) = s
gameState (GameOver s) = s

ordTurn = ordGameState . gameState
    where
        ordGameState GameState {player = Player{..}, boss = Boss{..}} = (playerHp, playerArmor, playerMana, bossHp)

nextTurn :: GameState -> GameState
nextTurn s@GameState{..} = s {turn = turn + 1}

applyEffects :: GameState -> GameState
applyEffects s@GameState {player = player@Player{..}, effects = effects} =
    foldl apply (s {player = player {playerArmor = 0}, effects = []}) effects
  where
    apply s@(GameState {player = player@Player{..}, effects = effects}) (t, Shield) =
        s {player = player {playerArmor = 7}, effects = (t - 1, Shield):effects }
    apply s@(GameState {boss = boss@Boss{..}, effects = effects }) (t, Poison) =
        s {boss = boss {bossHp = bossHp - 3}, effects = (t - 1, Poison):effects }
    apply s@(GameState {player = player@Player{..}, effects = effects}) (t, Recharge) =
        s {player = player { playerMana = playerMana + 101}, effects = (t - 1, Recharge):effects }

applySpell :: GameState -> Spell -> GameState
applySpell s@(GameState {player = player@Player{..},
                         playerManaSpent = playerManaSpent,
                         boss = boss@Boss{..},
                         effects = effects
                        }) = \case
    MagicMissile ->
        s {player = player {playerMana = playerMana - 53},
           playerManaSpent = playerManaSpent + 53,
           boss = boss {bossHp = bossHp - 4}
          }
    Drain ->
        s {player = player {playerHp = playerHp + 2, playerMana = playerMana - 73},
           playerManaSpent = playerManaSpent + 73,
           boss = boss {bossHp = bossHp - 2}
          }
    Shield ->
        s {player = player {playerMana = playerMana - 113},
           playerManaSpent = playerManaSpent + 113,
           effects = (6, Shield):effects
          }
    Poison ->
        s {player = player {playerMana = playerMana - 173},
           playerManaSpent = playerManaSpent + 173,
           effects = (6, Poison):effects
          }
    Recharge ->
        s {player = player {playerMana = playerMana - 229},
           playerManaSpent = playerManaSpent + 229,
           effects = (5, Recharge):effects
          }

-- Remove effects that have ended
filterEffects s@GameState{..} = s { effects = filter (\(t, _) -> t > 0) effects }

-- Game modes (for Part 2)
type Mode = GameState -> GameState

easy, hard :: Mode
easy = id
hard s@GameState {player = player@Player{..}} = s {player = player {playerHp = playerHp - 1}}

-- Player strategy: pick an effect that's not active
strat GameState{..} = [MagicMissile, Drain, Shield, Poison, Recharge] \\ activeEffects
    where activeEffects = map snd effects

tick :: Mode -> Turn -> [Turn]
tick mode (GameOver s) = []
tick mode (PlayerTurn s)
    | playerHp <= 0 = [GameOver s']
    | bossHp <= 0 = [GameOver s']
    | otherwise = do
        spell <- playerStrategy s'
        let s''@GameState {player = Player {playerMana = playerMana}} = applySpell s' spell
        if playerMana < 0
            then return $ GameOver s''
            else return $ BossTurn (nextTurn s'')
  where s'@GameState {player = player@Player{..}, boss = Boss{..}} = mode $ filterEffects $ applyEffects s
tick mode (BossTurn s)
    | playerHp <= 0 = [GameOver s']
    | bossHp <= 0 = [GameOver s']
    | otherwise =
        let damage = max 1 (bossDamage - playerArmor)
        in return $ PlayerTurn (nextTurn (s' {player = player {playerHp = playerHp - damage}}))
  where s'@GameState{ player = player@Player{..}, boss = boss@Boss{..}} = filterEffects $ applyEffects s

playerWin :: Turn -> Bool
playerWin (GameOver s@GameState{player = Player{..}, boss = Boss{..}}) =
    playerHp > 0 && playerMana >= 0 && bossHp <= 0
playerWin _ = False

bfs :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfs rep next start = loop Set.empty (Seq.fromList [start])
    where
        loop _    Empty = []
        loop seen (x :<| q1)
            | Set.member r seen = loop seen q1
            | otherwise = x : loop seen1 q2
            where
                r = rep x
                seen1 = Set.insert r seen
                q2 = q1 <> Seq.fromList (next x)

main = do
    let player = Player {playerHp = 50, playerArmor = 0, playerMana = 500, playerStrategy = strat}
        boss = Boss {bossHp = 71, bossDamage = 10}
    
    let s0 = GameState {player = player, playerManaSpent = 0, boss = boss, turn = 1, effects = []}
        turn = PlayerTurn s0

    -- Part 1
    print $ minimum $ map (playerManaSpent . gameState) $ filter playerWin $ bfs ordTurn (tick easy) turn

    -- Part 2
    print $ minimum $ map (playerManaSpent . gameState) $ filter playerWin $ bfs ordTurn (tick hard) turn
