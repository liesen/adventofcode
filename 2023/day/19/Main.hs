import Control.Arrow hiding ((+++))
import Data.Char
import Data.Either
import Data.Functor
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Debug.Trace
import Text.ParserCombinators.ReadP

-- workflowName{cond,cond,...,terminal}
newtype WorkflowName = WorkflowName String deriving (Eq, Ord)

instance Show WorkflowName where
  show (WorkflowName s) = s

data Workflow = Workflow WorkflowName [Rule] (Either WorkflowName Result) deriving (Show)

data Result = Accepted | Rejected deriving (Eq)

instance Show Result where
  show Accepted = "A"
  show Rejected = "R"

data Rule = Rule Cond (Either WorkflowName Result)

instance Show Rule where
  show (Rule cond (Left wf)) = show cond ++ ":" ++ show wf
  show (Rule cond (Right res)) = show cond ++ ":" ++ show res

data Cond = Cond Category Ordering Int

instance Show Cond where
  show (Cond k LT n) = show k ++ "<" ++ show n
  show (Cond k GT n) = show k ++ ">" ++ show n

data Category = X | M | A | S deriving (Eq, Ord)

instance Show Category where
  show X = "x"
  show M = "m"
  show A = "a"
  show S = "s"

parseResult = (char 'A' $> Accepted) +++ (char 'R' $> Rejected)

parseCond = Cond <$> parseCategory <*> parseOrdering <*> parseInt

parseCategory = choice [char 'x' $> X, char 'm' $> M, char 'a' $> A, char 's' $> S]

parseOrdering = choice [char '<' $> LT, char '>' $> GT]

parseInt = read <$> many1 (satisfy isDigit)

parseTerm = (Left <$> parseWorkflowName) +++ (Right <$> parseResult)

parseWorkflowName = WorkflowName <$> many1 (satisfy isAsciiLower)

parseWorkflow = do
  name <- parseWorkflowName
  between (char '{') (char '}') $ Workflow name <$> endBy parseRule (char ',') <*> parseTerm

parseRule = Rule <$> parseCond <*> (char ':' *> parseTerm)

newtype Part a = Part (Map Category a)

instance (Show a) => Show (Part a) where
  show (Part ratings) = "{" ++ intercalate "," [show k ++ "=" ++ show v | (k, v) <- Map.assocs ratings] ++ "}"

parsePart :: ReadP (Part Int)
parsePart = Part . Map.fromList <$> between (char '{') (char '}') (sepBy ((,) <$> parseCategory <*> (char '=' *> parseInt)) (char ','))

parse = do
  workflows <- endBy parseWorkflow (char '\n')
  skipSpaces
  ratings <- sepBy parsePart (char '\n')
  skipSpaces
  eof
  pure (workflows, ratings)

part1 :: Map WorkflowName Workflow -> Part Int -> Result
part1 workflows = send (WorkflowName "in")
  where
    send wf part =
      -- If a part is accepted (sent to A) or rejected (sent to R), the part immediately stops any further processing.
      case head (mapMaybe (checkRule part) rules ++ [result]) of
        Left wf' -> send wf' part
        Right res -> res
      where
        Workflow _ rules result = workflows ! wf

    checkRule (Part ratings) (Rule (Cond k cmp n) res)
      | compare (ratings ! k) n == cmp = return res
      | otherwise = fail "Condition does not hold"

part2 :: Map WorkflowName Workflow -> Part (Int, Int) -> [Part (Int, Int)]
part2 workflows = go (WorkflowName "in")
  where
    go wf part@(Part ratings)
      | any (\(lo, hi) -> lo >= hi) ratings = fail "Unsatisfiable range requirements"
      | otherwise =
          let (mismatch, matches) = mapAccumL checkRule part rules
           in concat matches ++ send result mismatch
      where
        Workflow _ rules result = workflows ! wf

    send (Left wf) part = go wf part
    send (Right Accepted) part = return part
    send (Right Rejected) part = fail "Rejected"

    adjust (Part ratings) k f = Part $ Map.adjust f k ratings

    checkRule part r@(Rule (Cond k cmp n) result)
      | cmp == LT =
          let match = adjust part k $ \(lo, hi) -> (lo, min hi (n - 1))
              mismatch = adjust part k $ \(lo, hi) -> (max lo n, hi)
           in (mismatch, send result match)
      | cmp == GT =
          let match = adjust part k $ \(lo, hi) -> (max lo (n + 1), hi)
              mismatch = adjust part k $ \(lo, hi) -> (lo, min hi n)
           in (mismatch, send result match)

score1 = sum . map (\(Part ratings) -> sum ratings)

score2 (Part ratings) = product (fmap (\(lo, hi) -> hi - lo + 1) ratings)

main =
  do
    input <- readFile "input"
    let [((workflows, parts), "")] = readP_to_S parse input
        workflowsMap = Map.fromList [(name, wf) | wf@(Workflow name _ _) <- workflows]

    -- Part 1
    print $ score1 $ filter ((== Accepted) . part1 workflowsMap) parts

    -- Part 2
    let wf2 = Part $ Map.fromList [(X, (1, 4000)), (M, (1, 4000)), (A, (1, 4000)), (S, (1, 4000))]
    print $ sum $ map score2 $ part2 workflowsMap wf2
