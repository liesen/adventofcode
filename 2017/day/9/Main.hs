module Main where

import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

data Thing = Garbage String
           | Group [Thing]
           | Text String
  deriving (Show, Eq)

escape = do
  _ <- char '!'
  _ <- anyChar
  return ""

garbage = Garbage <$> between (char '<') (char '>') text
  where
    text = concat <$> many (escape <|> (return <$> noneOf "!>"))

text = Text <$> concat <$> many1 (escape <|> (return <$> noneOf "!{}<"))

group = Group <$> between (char '{') (char '}') (many (group <|> garbage <|> text))

tests = [
    ("<>", Garbage ""),
    ("<random characters>", Garbage "random characters"),
    ("<<<<>", Garbage "<<<"),
    ("<{!>}>", Garbage "{}"),
    ("<!!>", Garbage ""),
    ("<!!!>>", Garbage ""),
    ("<{o\"i!a,<{i<a>", Garbage "{o\"i,<{i<a"),
    ("{}", Group []),
    ("{{{}}}", Group [Group [Group []]]),
    ("{{}{}}", Group [Group [], Group []]),
    ("{{<a>},{<a>},{<a>},{<a>}}", Group [Group [Group [Group [Group []]]]]),
    ("{{<!>},{<!>},{<!>},{<a>}}", Group [])
  ]

test = forM_ tests $ \(s, res) -> do
         putStrLn s
         case parse ((garbage <|> group) <* eof) "" s of
           Left err -> print err
           Right g -> do
             print g
             print (score g)
         putStrLn ""

score = score' 1
  where
    score' n (Group xs) = n + sum (map (score' (n + 1)) xs)
    score' _ _          = 0

countGarbage (Group xs)  = sum (map countGarbage xs)
countGarbage (Garbage s) = length s
countGarbage (Text _)    = 0

main = do
    input <- readFile "input.txt"
    
    case parse (group <* endOfLine <* eof) "" input of
      Left err -> print err
      Right g  -> do
        -- Part 1
        print (score g)
    
        -- Part 2
        print (countGarbage g)
