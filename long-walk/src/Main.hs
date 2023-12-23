module Main (main) where

import Vector2D (Vector2D, Coord2D, at)
import qualified Vector2D
import qualified Data.Vector as Vector
import Data.Maybe (fromMaybe)
import Data.Foldable (maximumBy)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let hikeMap = parseInput input
  let start = startCoord hikeMap
  let finish = finishCoord hikeMap
  let allWalks = take 40000 $ walk start finish 0 hikeMap
  let (maxLen, longestWalkMap) = maximumBy (\a b -> compare (fst a) (fst b)) allWalks
  print maxLen
  putStrLn $ showMap longestWalkMap

parseInput :: String -> Vector2D Char
parseInput = Vector2D.fromList . lines

showMap :: Vector2D Char -> String
showMap = unlines . Vector2D.toList

startCoord :: Vector2D Char -> Coord2D
startCoord vector = fromMaybe (0,0) $ Vector2D.findCoord (== '.') vector

finishCoord :: Vector2D Char -> Coord2D
finishCoord vector = (height-1, x)
  where
    x = fromMaybe 0 $ Vector.findIndex (== '.') $ Vector.last vector
    (height, _width) = Vector2D.size vector

walk :: Coord2D -> Coord2D -> Int -> Vector2D Char -> [(Int, Vector2D Char)]
walk start finish n hikeMap = case possibleSteps start hikeMap' of
    [] -> [(n, hikeMap') | start == finish]
    coords -> concatMap (\c -> walk c finish (n+1) hikeMap') coords
  where
    hikeMap' = Vector2D.setAt start 'O' hikeMap

possibleSteps :: Coord2D -> Vector2D Char -> [Coord2D]
possibleSteps coord hmap = map snd $ filter (isValidStep hmap) $ possibleDirections coord

isValidStep :: Vector2D Char -> (Dir, Coord2D) -> Bool
isValidStep hmap (dir, coord) = case (hmap `at` coord, dir) of
  (Just '.', _) -> True
  (Just '>', _) -> True
  (Just '<', _) -> True
  (Just '^', _) -> True
  (Just 'v', _) -> True
  _ -> False

data Dir = L | R | U | D deriving (Show, Eq)

possibleDirections :: Coord2D-> [(Dir, Coord2D)]
possibleDirections (x,y) = [(D, (x+1,y)), (U, (x-1,y)), (R, (x,y+1)), (L, (x,y-1))]
