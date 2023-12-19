import Data.Char
import Data.Functor
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe
import Text.ParserCombinators.ReadP

-- workflowName{cond,cond,...,terminal}
newtype WorkflowName = WorkflowName String deriving (Eq, Ord, Show)

data Workflow = Workflow WorkflowName [Rule] (Either WorkflowName Result) deriving (Show)

data Result = Accepted | Rejected deriving (Eq, Show)

data Rule = Rule Cond (Either WorkflowName Result) deriving (Show)

data Cond = Cond Category Ordering Int deriving (Show)

data Category = X | M | A | S deriving (Eq, Ord, Show)

parseResult = (char 'A' $> Accepted) +++ (char 'R' $> Rejected)

parseCond = Cond <$> parseCategory <*> parseOrdering <*> parseInt

parseCategory = choice [char 'x' $> X, char 'm' $> M, char 'a' $> A, char 's' $> S]

parseOrdering = choice [char '<' $> LT, char '>' $> GT]

parseInt = read <$> many1 (satisfy isDigit)

parseTerm = (Left <$> parseWorkflowName) +++ (Right <$> parseResult)

parseWorkflowName = WorkflowName <$> many1 (satisfy isAsciiLower)

parseWorkflow = do
  name <- parseWorkflowName
  between (char '{') (char '}') $ Workflow name <$> endBy parseRule (char ',') <*>  parseTerm

parseRule = Rule <$> parseCond <*> (char ':' *> parseTerm)

newtype Part = Part (Map Category Int) deriving (Show)

parsePart :: ReadP Part
parsePart = Part . Map.fromList <$> between (char '{') (char '}') (sepBy ((,) <$> parseCategory <*> (char '=' *> parseInt)) (char ','))

parse = do
  workflows <- endBy parseWorkflow (char '\n')
  skipSpaces
  ratings <- sepBy parsePart (char '\n')
  skipSpaces
  eof
  pure (workflows, ratings)

sort' :: Map WorkflowName Workflow -> Part -> Result
sort' workflows = go (WorkflowName "in")
  where
    go wf part =
      case head (mapMaybe (rule part) rules ++ [result]) of
        Left wf' -> go wf' part
        Right res -> res
      where
        Just (Workflow _ rules result) = Map.lookup wf workflows
        rule part (Rule cond result)
          | check cond part = Just result
          | otherwise = Nothing
        check (Cond cat cmp val) (Part ratings) = compare (ratings ! cat) val == cmp

main = do
  input <- readFile "input"
  let [((workflows, parts), "")] = readP_to_S parse input
      workflowsMap = Map.fromList [(name, wf) | wf@(Workflow name _ _) <- workflows]

  -- Part 1
  print $ sum $ map (\(Part ratings) -> sum ratings) $ filter ((== Accepted) . sort' workflowsMap) parts
