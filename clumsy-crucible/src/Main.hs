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
  let (cst, path) = findPath (0, 0) vector
  putStrLn $ drawPath path vector
  print cst

parseInput :: String -> Vector2D Int
parseInput str = Vector2D.fromList $ map digitToInt <$> lines str

drawPath :: [Coord2D] -> Vector2D Int -> String
drawPath coords vector = unlines $ Vector2D.toList $ Vector2D.map toChar $ foldl (\vect c -> setAt c 0 vect) vector coords
  where
    toChar 0 = '#'
    toChar _ = '.'

findPath :: Coord2D -> Vector2D Int -> (Int, [Coord2D])
findPath startCoord vector = findPath' [(0, [startCoord])] [] 10000
  where
    goal = goalCoord vector

    findPath' :: [(Int, [Coord2D])] -> [Coord2D] -> Int -> (Int, [Coord2D])
    findPath' [] _ _ = (0, [])
    findPath' ((costSoFar, path):paths) visited n
      | head path == goal = (costSoFar, path)
      | n == 0 = (costSoFar, path)
      | otherwise = findPath' (sortOn fst $ [(cost vector c + costSoFar, c:path) | c <- nextCoords path vector visited] ++ paths) (head path : visited) (n-1)

-- cost of traveling through a coordinate
cost :: Vector2D Int -> Coord2D -> Int
cost vect coord = fromMaybe 1000000 (vect `at` coord)

goalCoord :: Vector2D a -> Coord2D
goalCoord v = case Vector2D.size v of
  (x, y) -> (x-1, y-1)

nextCoords :: [Coord2D] -> Vector2D a -> [Coord2D] -> [Coord2D]
nextCoords [] _ _ = []
nextCoords ((x,y):coords) vector visited = filter isValidCoord [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
  where
    isValidCoord coord = case vector `at` coord of
      Nothing -> False -- can't step outside of the map
      Just _ -> coord `notElem` visited && -- can't go to already visited coordinate
                not (has5Straight (coord:(x,y):coords)) -- can't do > 3 steps in the same direction

has5Straight :: [Coord2D] -> Bool
has5Straight coords
  | length coords < 5 = False
  | otherwise = isStraight $ take 4 coords

isStraight :: [Coord2D] -> Bool
isStraight (a:b:coords) = all (==firstDir) $ zipWith direction (b:coords) coords
  where firstDir = direction a b
isStraight _ = True

data Direction = Horiz | Vert deriving Eq

direction :: Coord2D -> Coord2D -> Direction
direction (x,_) (x',_) = if x == x' then Horiz else Vert

