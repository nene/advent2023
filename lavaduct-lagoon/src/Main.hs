module Main (main) where

import Vector2D (Coord2D, Vector2D)
import qualified Vector2D
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let path = toCanonicalCoords $ createPath $ parseInput input
  let filledVect = floodFill (78,1) '#' $ drawPath path
  putStrLn $ showVector2D filledVect
  print $ countElem '#' $ unlines $ Vector2D.toList filledVect

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

showVector2D :: Vector2D Char -> String
showVector2D vector = unlines $ transpose $ Vector2D.toList vector

floodFill :: Eq a => Coord2D -> a -> Vector2D a -> Vector2D a
floodFill (x,y) value vector = case vector `Vector2D.at` (x,y) of
  Nothing -> vector
  Just v ->
    if v == value
    then vector
    else foldl (\vec c -> floodFill c value vec) filledVect surroundingCoords
      where
        filledVect = Vector2D.setAt (x,y) value vector
        surroundingCoords = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

countElem :: Eq a => a -> [a] -> Int
countElem v xs = foldl (\cnt e -> if e == v then cnt+1 else cnt) 0 xs