module Main (main) where

import Vector3D (Coord3D)
import qualified Vector3D
import Data.List.Utils (split)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let bricks = parseInput input
  print bricks

parseInput :: String -> [(Coord3D, Coord3D)]
parseInput input = parseLine <$> lines input
  where
    parseLine line = case split "~" line of
      [left, right] -> (parseCoord left, parseCoord right)
      _ -> error "Invalid line"

    parseCoord txt = case split "," txt of
      [x,y,z] -> (read x, read y, read z)
      _ -> error "Invalid coordinate"
