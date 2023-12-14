module Main (main) where

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ lines input

