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
  let solutions = findPath (0, 0) vector
  putStrLn $ unlines $ (\(cst, p) -> drawPath p vector ++ show cst ++ "\n") <$> solutions
  print $ length solutions

parseInput :: String -> Vector2D Int
parseInput str = Vector2D.fromList $ map digitToInt <$> lines str

drawPath :: [Coord2D] -> Vector2D Int -> String
drawPath coords vector = unlines $ Vector2D.toList $ Vector2D.map toChar $ foldl (\vect c -> setAt c 0 vect) vector coords
  where
    toChar 0 = '#'
    toChar _ = '.'

findPath :: Coord2D -> Vector2D Int -> [(Int, [Coord2D])]
findPath startCoord vector = findPath' [(0, [startCoord])] 6
  where
    goal = goalCoord vector

    findPath' :: [(Int, [Coord2D])] -> Int -> [(Int, [Coord2D])]
    findPath' [] _ = [(0, [])]
    findPath' ((costSoFar, path):paths) n
      | head path == goal = (costSoFar, path):paths
      | n == 0 = (costSoFar, path):paths
      | otherwise = findPath' (sortOn fst $ nextPaths ++ paths) (n-1)
        where
          nextPaths = [(cost vector c + costSoFar, c:path) | c <- nextCoords path vector]

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
      Just _ -> coord `notElem` coords && -- can't go to already visited coordinate
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

