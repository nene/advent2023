module Main (main) where

import Vector2D (Vector2D, Coord2D, at)
import qualified Vector2D
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let hikeMap = parseInput input
  let start = startCoord hikeMap
  print $ possibleSteps start hikeMap

parseInput :: String -> Vector2D Char
parseInput = Vector2D.fromList . lines

startCoord :: Vector2D Char -> Coord2D
startCoord vector = fromMaybe (0,0) $ Vector2D.findCoord (== '.') vector

-- walk :: Coord2D -> Vector2D Char -> Int
-- walk startAt hikeMap =
--   where
--     stepsAt coord hmap = 


possibleSteps :: Coord2D -> Vector2D Char -> [Coord2D]
possibleSteps coord hmap = map snd $ filter (isValidStep hmap) $ possibleDirections coord

isValidStep :: Vector2D Char -> (Dir, Coord2D) -> Bool
isValidStep hmap (dir, coord) = case (hmap `at` coord, dir) of
  (Just '.', _) -> True
  (Just '>', R) -> True
  (Just '<', L) -> True
  (Just '^', U) -> True
  (Just 'v', D) -> True
  _ -> False

data Dir = L | R | U | D deriving (Show, Eq)

possibleDirections :: Coord2D-> [(Dir, Coord2D)]
possibleDirections (x,y) = [(D, (x+1,y)), (U, (x-1,y)), (R, (x,y+1)), (L, (x,y-1))]
