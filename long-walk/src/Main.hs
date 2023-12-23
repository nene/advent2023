module Main (main) where

import Vector2D (Vector2D)
import qualified Vector2D

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ parseInput input

parseInput :: String -> Vector2D Char
parseInput = Vector2D.fromList . lines
