module Main (main) where

import Vector2D (Vector2D, Coord2D, at)
import qualified Vector2D
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.List.Utils (uniq)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (gardenPlan, startCoord) = parseInput input
  print startCoord
  let n = 100
  putStrLn $ "After " ++ show n ++ " steps:"
  print $ length $ stepN n startCoord gardenPlan

parseInput :: String -> (Vector2D Char, Coord2D)
parseInput input = (cleanVector, startCoord)
  where
    cleanVector = Vector2D.map (\v -> if v == 'S' then '.' else v) rawVector
    startCoord = fromMaybe (-1, -1) $ Vector2D.findCoord (=='S') rawVector
    rawVector = Vector2D.fromList $ lines input

stepN :: Int -> Coord2D -> Vector2D Char -> [Coord2D]
stepN n startCoord vector = iterate (`stepAll` vector) [startCoord] !! n

stepAll :: [Coord2D] -> Vector2D Char -> [Coord2D]
stepAll coords vector = uniq $ sort $ concatMap (`step` vector) coords

step :: Coord2D -> Vector2D Char -> [Coord2D]
step (x,y) vector = filter isEmptyPlot [(x+1,y), (x-1, y), (x,y+1), (x, y-1)]
  where
    isEmptyPlot coord = vector `infiniteAt` coord == Just '.'

infiniteAt :: Vector2D a -> Coord2D -> Maybe a
infiniteAt vector (x, y) = vector `at` (x `mod` sizeX, y `mod` sizeY)
  where
    (sizeX, sizeY) = Vector2D.size vector
