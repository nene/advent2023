module Main (main) where

import Vector2D (Coord2D, Vector2D)
import qualified Vector2D
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let path = toCanonicalCoords $ createPath $ parseInput input
  putStrLn $ showPath $ drawPath path

data Dir = L | R | U | D deriving (Eq, Show)

parseInput :: String -> [(Dir, Int)]
parseInput txt = parseLine <$> lines txt
  where
    parseLine s = case words s of
      ["L",i,_] -> (L, read i)
      ["R",i,_] -> (R, read i)
      ["U",i,_] -> (U, read i)
      ["D",i,_] -> (D, read i)
      _ -> error ("Invalid input: " ++ s)

createPath :: [(Dir, Int)] -> [Coord2D]
createPath = foldl extendLine [(0,0)]
  where
    extendLine :: [Coord2D] -> (Dir, Int) -> [Coord2D]
    extendLine [] _ = []
    extendLine (p:ps) cmd = line p cmd ++ ps

    line :: Coord2D -> (Dir, Int) -> [Coord2D]
    line (x,y) (R, len) = reverse [(x+dx, y) | dx <- [0..len]]
    line (x,y) (L, len) = reverse [(x-dx, y) | dx <- [0..len]]
    line (x,y) (D, len) = reverse [(x, y+dy) | dy <- [0..len]]
    line (x,y) (U, len) = reverse [(x, y-dy) | dy <- [0..len]]

minX :: [Coord2D] -> Int
minX coords = minimum $ fst <$> coords

minY :: [Coord2D] -> Int
minY coords = minimum $ snd <$> coords

maxX :: [Coord2D] -> Int
maxX coords = maximum $ fst <$> coords

maxY :: [Coord2D] -> Int
maxY coords = maximum $ snd <$> coords

toCanonicalCoords :: [Coord2D] -> [Coord2D]
toCanonicalCoords coords = toPositive <$> coords
  where
    toPositive (x,y) = (x-minx, y-miny)
    minx = minX coords
    miny = minY coords

drawPath :: [Coord2D] -> Vector2D Char
drawPath coords = foldl (\vec c -> Vector2D.setAt c '#' vec) emptyVector coords
  where
    emptyVector = Vector2D.fill (maxX coords + 1, maxY coords + 1) '.'

showPath :: Vector2D Char -> String
showPath vector = unlines $ transpose $ Vector2D.toList vector
