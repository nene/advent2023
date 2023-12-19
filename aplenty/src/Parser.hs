module Parser (parseInput) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.Utils (split)
import Text.ParserCombinators.Parsec

parseInput :: String -> (Map String [Rule], [Part])
parseInput input = case split "\n\n" input of
  [a,b] -> (Map.fromList (parseWorkflow <$> lines a), parsePart <$> lines b)
  _ -> error "Invalid input"

data Part = Part Int Int Int Int deriving (Show)

parsePart :: String -> Part
parsePart input = case parse part "(unknown)" input of
  Right p -> p
  Left _ -> error "Failed to parse part"

-- {x=787,m=2655,a=1222,s=2876}
part :: GenParser Char st Part
part = do
  _ <- string "{x="
  x <- many1 digit
  _ <- string ",m="
  m <- many1 digit
  _ <- string ",a="
  a <- many1 digit
  _ <- string ",s="
  s <- many1 digit
  _ <- string "}"
  return (Part (read x) (read m) (read a) (read s))

data Rule = Conditional PartType Op Int Action | Unconditional Action deriving (Show)

data PartType = X | M | A | S deriving (Show)

data Action = Goto String | Accept | Reject deriving (Show)

data Op = OpGT | OpLT deriving (Show)

parseWorkflow :: String -> (String, [Rule])
parseWorkflow input = case parse workflow "(unknown)" input of
  Right p -> p
  Left _ -> error "Failed to parse workflow"

-- px{a<2006:qkq,m>2090:A,rfg}
workflow :: GenParser Char st (String, [Rule])
workflow = do
  name <- many1 letter
  _ <- string "{"
  rules <- sepBy rule (char ',')
  _ <- string "}"
  return (name, rules)

rule :: GenParser Char st Rule
rule = conditionalRule <|> unconditionalRule

-- a<2006:qkq
conditionalRule :: GenParser Char st Rule
conditionalRule = do
  t <- partType
  op <- oneOf "<>"
  nr <- many1 digit
  _ <- char ':'
  Conditional t (if op == '>' then OpGT else OpLT) (read nr) <$> action

partType :: GenParser Char st PartType
partType = do
  c <- oneOf "xmas"
  return (partTypeFromChar c)

partTypeFromChar :: Char -> PartType
partTypeFromChar 'x' = X
partTypeFromChar 'm' = M
partTypeFromChar 'a' = A
partTypeFromChar 's' = S
partTypeFromChar c = error ("Invalid part type" ++ [c])

-- rfg  A   R
unconditionalRule :: GenParser Char st Rule
unconditionalRule = do
  Unconditional <$> action

action :: GenParser Char st Action
action = accept <|> reject <|> goto

accept :: GenParser Char st Action
accept = do
  _ <- char 'A'
  return Accept

reject :: GenParser Char st Action
reject = do
  _ <- char 'R'
  return Reject

goto :: GenParser Char st Action
goto = do
  name <- many1 letter
  return (Goto name)

