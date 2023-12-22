module Main (main) where

import Vector3D (Coord3D, Vector3D)
import qualified Vector3D
import qualified Vector2D
import Data.List.Utils (split)
import Data.Vector ((//))
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let bricks = parseInput input
  let size = inputSize bricks
  print bricks
  print size
  let cube = initialCube size
  print cube
  putStrLn $ visualizeCube cube

parseInput :: String -> [(Coord3D, Coord3D)]
parseInput input = parseLine <$> lines input
  where
    parseLine line = case split "~" line of
      [left, right] -> (parseCoord left, parseCoord right)
      _ -> error "Invalid line"

    parseCoord txt = case split "," txt of
      [z,y,x] -> (read x, read y, read z)
      _ -> error "Invalid coordinate"

visualizeCube :: Vector3D Int -> String
visualizeCube vect = unlines $ foldl1 sideWays $ planeToStrings <$> planes
  where
    sideWays :: [String] -> [String] -> [String]
    sideWays = zipWith (\x y -> x ++ " " ++ y)

    planeToStrings :: [[Int]] -> [String]
    planeToStrings plane = map intToChar <$> plane

    intToChar 0 = '.'
    intToChar (-1) = '-'
    intToChar _ = '#'

    planes :: [[[Int]]]
    planes = reverse <$> transpose (Vector3D.toList vect)


inputSize :: [(Coord3D, Coord3D)] -> Coord3D
inputSize pairs = (x+1, y+1, z+1)
  where (x, y, z) = maxCoord3D $ concatMap (\(a,b) -> [a, b]) pairs

maxCoord3D :: [Coord3D] -> Coord3D
maxCoord3D coords = (maxCoord Vector3D.coord3DX, maxCoord Vector3D.coord3DY, maxCoord Vector3D.coord3DZ)
  where maxCoord fn = maximum $ map fn coords

initialCube :: Coord3D -> Vector3D Int
initialCube (x,y,z) = emptyCube // [(0, filledFloor)]
  where
    emptyCube = Vector3D.fill (x,y,z) 0
    filledFloor = Vector2D.fill (y,z) (-1)

