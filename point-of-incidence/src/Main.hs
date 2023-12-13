module Main (main) where

import Data.List.Utils (split)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ parseInput input

type Puzzle = [String]

parseInput :: String -> [Puzzle]
parseInput input = lines <$> split "\n\n" input
