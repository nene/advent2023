module Main (main) where

import System.Environment (getArgs)
import Vector3D (Coord3D)
import Data.List.Utils (split)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  print $ parseInput input

-- 19, 13, 30 @ -2,  1, -2
parseInput :: String -> [(Coord3D, Coord3D)]
parseInput input = parseLine <$> lines input
  where
    parseLine ln = case split " @ " ln of
      [p, v] -> (parseCoord p, parseCoord v)
      _ -> error "Invalid input"
    
    parseCoord txt = case split ", " txt of
      [x,y,z] -> (read x, read y, read z)
      _ -> error "Invalid input"
