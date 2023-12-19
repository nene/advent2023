module Main (main) where

import Parser (parseInput)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (workflowMap, parts) = parseInput input
  putStrLn "Workflow map:"
  print workflowMap
  putStrLn ""
  putStrLn "Parts:"
  print parts
