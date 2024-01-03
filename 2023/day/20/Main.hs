{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Data.Char
import Data.Function (on)
import Data.Functor
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Text.ParserCombinators.ReadP

type Pulse = Bool

low, high :: Pulse
low = False
high = True

data Module
  = Broadcaster [String]
  | Flipflop Bool String [String]
  | Conjunction (Map String Pulse) String [String]

moduleName (Broadcaster _) = "broadcaster"
moduleName (Flipflop _ name _) = name
moduleName (Conjunction _ name _) = name

outputs (Broadcaster outs) = outs
outputs (Flipflop _ _ outs) = outs
outputs (Conjunction _ _ outs) = outs

instance Show Module where
  show (Broadcaster outs) = "broadcaster" ++ " -> " ++ intercalate ", " outs
  show (Flipflop value name outs) = '%' : name ++ " -> " ++ intercalate ", " outs
  show (Conjunction memory name outs) = '&' : name ++ " -> " ++ intercalate ", " outs

parseModule :: ReadP Module
parseModule = moduleType <*> (string " -> " *> sepBy moduleName (string ", "))
  where
    moduleName = many1 (satisfy isAlpha)
    moduleType =
      choice
        [ string "broadcaster" $> Broadcaster,
          Flipflop False <$> (char '%' *> moduleName),
          Conjunction mempty <$> (char '&' *> moduleName)
        ]

-- Update the state machine and accumulate new pulses
emit state (src, pulse, dst) =
  case Map.lookup dst state of
    Nothing -> (state, []) -- Pulse sent to an untyped (noop) module
    Just (Broadcaster outs) -> (state, map ("broadcaster",pulse,) outs)
    Just (Flipflop value _ outs)
      -- If a flip-flop module receives a high pulse, it is ignored and
      -- nothing happens.
      | pulse == high -> (state, [])
      -- If it was off, it turns on and sends a high pulse. If it was on,
      -- it turns off and sends a low pulse.
      | otherwise ->
          let value' = not value
              pulse' = value'
              state' = Map.insert dst (Flipflop value' dst outs) state
           in (state', map (dst,pulse',) outs)
    Just (Conjunction memory _ outs) ->
      -- If a conjunction module remembers high pulses for all inputs, it
      -- sends a low pulse; otherwise, it sends a high pulse.
      let memory' = Map.insert src pulse memory
          pulse' = not (and memory')
          state' = Map.insert dst (Conjunction memory' dst outs) state
       in (state', map (dst,pulse',) outs)

-- Let convolutions know what their inputs are
rewire modules = map f modules
  where
    f (Conjunction memory name outs) = Conjunction memory' name outs
      where
        memory' = Map.fromList [(moduleName m, low) | m <- modules, name `elem` outputs m]
    f m = m

-- Ah, push it
push s =
  let (s', hist, _todo) = until done propagate (s, mempty, [("button", low, "broadcaster")])
   in (s', hist)
  where
    done (s, hist, todo) = null todo
    propagate (s, hist, todo) = let (s', concat -> todo') = mapAccumL emit s todo in (s', hist <> todo, todo')

-- All you fly mothers, get out on there and dance
simulate s = iterate (\(s, _hist) -> push s) (s, [])

-- Sum all the (low, high) pulses emitted each push
pulseCounter = scanl (foldl f) (0, 0) . map snd . simulate
  where
    f (nlo, nhi) (_, pulse, _)
      | pulse = (nlo, nhi + 1)
      | otherwise = (nlo + 1, nhi)

dot modules = unlines $ ["digraph M {"] ++ header ++ body ++ ["}"]
  where
    header = flip fmap modules $ \case
      Broadcaster outs -> "  broadcaster [shape=doubleoctagon]"
      Flipflop _ name outs -> "  " ++ name ++ " [shape=triangle]"
      Conjunction _ name outs -> "  " ++ name ++ " [shape=diamond]"
    body = flip fmap modules $ \case
      Broadcaster outs -> "  broadcaster -> {" ++ intercalate ", " outs ++ "};"
      Flipflop _ name outs -> "  " ++ name ++ " -> {" ++ intercalate ", " outs ++ "};"
      Conjunction _ name outs -> "  " ++ name ++ " -> {" ++ intercalate ", " outs ++ "};"

main = do
  input <- readFile "input"
  let [(modules, "")] = readP_to_S (sepBy parseModule (char '\n') <* skipSpaces <* eof) input
      modules' = rewire modules

  -- Part 1
  let s = Map.fromList [(moduleName mod, mod) | mod <- modules']
      (nlo, nhi) = drop 1 (pulseCounter s) !! 1000  -- Drop the initial state
  print $ nlo * nhi

  -- Part 2: push it real good!
  --
  -- From inspection: the rx module has a single input, rx', which in turn
  -- has four inputs. rx' is a conjunction, so for it to emit a low pulse,
  -- all its inputs must emit a high pulse
  let Just rx'@(Conjunction mem _ _) = find (\case (Conjunction m k ["rx"]) -> True; _ -> False) modules'
      inputs = Map.keys mem

  -- Find time until rx's inputs emits a high pulse, then when all inputs
  -- will do that at the same time
  let findHighPulse k =
        let p (src, pulse, dst) = src == k && pulse == high
         in length (takeWhile (not . any p . snd) (simulate s))
  print $ foldl1 lcm $ map findHighPulse inputs
