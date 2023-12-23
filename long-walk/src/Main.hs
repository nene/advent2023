module Main (main) where

import Vector2D (Vector2D, Coord2D)
import qualified Vector2D
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let hikeMap = parseInput input
  print $ startCoord hikeMap

parseInput :: String -> Vector2D Char
parseInput = Vector2D.fromList . lines

startCoord :: Vector2D Char -> Coord2D
startCoord vector = fromMaybe (0,0) $ Vector2D.findCoord (== '.') vector

