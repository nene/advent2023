module Main (main) where

import Vector3D (Coord3D)
import qualified Vector3D
import Data.List.Utils (split)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let bricks = parseInput input
  let size = inputSize bricks
  print bricks
  print size

parseInput :: String -> [(Coord3D, Coord3D)]
parseInput input = parseLine <$> lines input
  where
    parseLine line = case split "~" line of
      [left, right] -> (parseCoord left, parseCoord right)
      _ -> error "Invalid line"

    parseCoord txt = case split "," txt of
      [x,y,z] -> (read x, read y, read z)
      _ -> error "Invalid coordinate"

inputSize :: [(Coord3D, Coord3D)] -> Coord3D
inputSize pairs = (x+1, y+1, z+1)
  where (x, y, z) = maxCoord3D $ concatMap (\(a,b) -> [a, b]) pairs

maxCoord3D :: [Coord3D] -> Coord3D
maxCoord3D coords = (maxCoord Vector3D.coord3DX, maxCoord Vector3D.coord3DY, maxCoord Vector3D.coord3DZ)
  where maxCoord fn = maximum $ map fn coords
