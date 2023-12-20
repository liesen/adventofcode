{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Debug.Trace
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

push1 (state, counter) =
  fst $
    until
      (null . snd)
      ( \((state, counter), pulses) ->
          let (state', concat -> pulses') = mapAccumL emit state pulses
              -- Count up the pulses
              counter' = foldl (\(lo, hi) (_, pulse, _) -> (lo + if not pulse then 1 else 0, hi + if pulse then 1 else 0)) counter pulses
           in ((state', counter'), pulses')
      )
      ((state, counter), [("button", low, "broadcaster")])

push2 (state, done) =
  fst $
    until
      (null . snd)
      ( \((state, done), pulses) ->
          let (state', concat -> pulses') = mapAccumL emit state pulses
              done' = done || any (\(_sender, pulse, receiver) -> pulse == low && receiver == "rx") pulses
           in ((state', done'), pulses')
      )
      ((state, done), [("button", low, "broadcaster")])

emit state (sender, pulse, receiver) =
  -- trace ("> " ++ sender ++ " -" ++ show pulse ++ "-> " ++ receiver) $
  case Map.lookup receiver state of
    Nothing -> (state, [])
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
              state' = Map.insert receiver (Flipflop value' receiver outs) state
           in (state', map (receiver,pulse',) outs)
    Just (Conjunction memory _ outs) ->
      -- If a conjunction module remembers high pulses for all inputs, it
      -- sends a low pulse; otherwise, it sends a high pulse.
      let memory' = Map.insert sender pulse memory
          pulse' = not (and memory')
          state' = Map.insert receiver (Conjunction memory' receiver outs) state
       in (state', map (receiver,pulse',) outs)

-- Let convolutions know what their inputs are
rewire modules = map f modules
  where
    f (Conjunction memory name outs) = Conjunction memory' name outs
      where
        memory' =
          Map.fromList
            [ (moduleName m, low)
              | m <- modules,
                name `elem` outputs m
            ]
    f m = m

main = do
  input <- readFile "input"
  let [(modules, "")] = readP_to_S (sepBy parseModule (char '\n') <* skipSpaces <* eof) input
      modules' = rewire modules

  let m = Map.fromList [(moduleName mod, mod) | mod <- modules']
  let (_, (lows, highs)) = iterate push1 (m, (0, 0)) !! 1000
  print $ lows * highs

-- Nope...
-- print $ length $ dropWhile (not . snd) $ iterate push2 (m, False)
