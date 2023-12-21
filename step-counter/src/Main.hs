module Main (main) where

import Vector2D (Vector2D, Coord2D)
import qualified Vector2D
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (gardenPlan, startCoord) = parseInput input
  print startCoord
  print gardenPlan

parseInput :: String -> (Vector2D Char, Coord2D)
parseInput input = (cleanVector, startCoord)
  where
    cleanVector = Vector2D.map (\v -> if v == 'S' then '.' else v) rawVector
    startCoord = fromMaybe (-1, -1) $ Vector2D.findCoord (=='S') rawVector
    rawVector = Vector2D.fromList $ lines input
