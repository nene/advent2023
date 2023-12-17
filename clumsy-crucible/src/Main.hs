module Main (main) where

import Vector2D (Vector2D)
import qualified Vector2D
import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ parseInput input

parseInput :: String -> Vector2D Int
parseInput str = Vector2D.fromList $ map digitToInt <$> lines str
