module Main (main) where

import Parser (parseInput, Part (Part), WorkflowMap, Rule (Unconditional, Conditional), Action (Accept, Reject, Goto), PartType (X, M, A, S), Op (OpLT, OpGT))
import Data.Map.Strict ((!))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (workflows, parts) = parseInput input
  print $ sum $ partValue <$> filter (isAccepted workflows "in") parts

isAccepted :: WorkflowMap -> String -> Part -> Bool
isAccepted workflows key part = case doWorkflow (workflows ! key) part of
  Accept -> True
  Reject -> False
  Goto name -> isAccepted workflows name part

doWorkflow :: [Rule] -> Part -> Action
doWorkflow [] _part = Reject
doWorkflow ((Unconditional action):_rules) _part = action
doWorkflow ((Conditional partType OpLT value action):rules) part =
  if partTypeValue partType part < value then action else doWorkflow rules part
doWorkflow ((Conditional partType OpGT value action):rules) part =
  if partTypeValue partType part > value then action else doWorkflow rules part

partTypeValue :: PartType -> Part -> Int
partTypeValue X (Part x _ _ _) = x
partTypeValue M (Part _ m _ _) = m
partTypeValue A (Part _ _ a _) = a
partTypeValue S (Part _ _ _ s) = s

partValue :: Part -> Int
partValue (Part x m a s) = x + m + a + s
