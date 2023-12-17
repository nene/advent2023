module Main (main) where

import Vector2D (Vector2D, Coord2D, at, setAt)
import qualified Vector2D
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.List (sortOn)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let vector = parseInput input
  putStrLn $ drawPath (findPath (0, 0) vector) vector

parseInput :: String -> Vector2D Int
parseInput str = Vector2D.fromList $ map digitToInt <$> lines str

drawPath :: [Coord2D] -> Vector2D Int -> String
drawPath coords vector = unlines $ Vector2D.toList $ Vector2D.map toChar $ foldl (\vect c -> setAt c 0 vect) vector coords
  where
    toChar 0 = '#'
    toChar _ = '.'

findPath :: Coord2D -> Vector2D Int -> [Coord2D]
findPath startCoord vector = findPath' [(0, [startCoord])] 1000
  where
    goal = goalCoord vector
    heur = heuristic goal

    findPath' :: [(Int, [Coord2D])] -> Int -> [Coord2D]
    findPath' [] _ = []
    findPath' ((costSoFar, path):paths) n
      | head path == goal = path
      | n == 0 = path
      | otherwise = findPath' (sortOn estimPathCost $ [(cost vector c + costSoFar, c:path) | c <- nextCoords path vector] ++ paths) (n-1)

    estimPathCost (pathCost, path) = pathCost + heur (head path)

-- estimates cheapest path from a coordinate to goal coordinate
heuristic :: Coord2D -> Coord2D -> Int
heuristic (goalX, goalY) (x, y) = 9 * (goalX - x + goalY - y)

-- cost of traveling through a coordinate
cost :: Vector2D Int -> Coord2D -> Int
cost vect coord = fromMaybe 1000000 (vect `at` coord)

goalCoord :: Vector2D a -> Coord2D
goalCoord v = case Vector2D.size v of
  (x, y) -> (x-1, y-1)

nextCoords :: [Coord2D] -> Vector2D a -> [Coord2D]
nextCoords [] _ = []
nextCoords ((x,y):coords) vector = filter isValidCoord [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
  where
    isValidCoord coord = case vector `at` coord of
      Nothing -> False -- can't step outside of the map
      Just _ -> coord `notElem` coords -- can't go to already visited coordinate
