module Main (main) where

import Vector2D (Vector2D, Coord2D, at, updateAt)
import qualified Vector2D
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  input <- readFile "input.txt"
  stepThrough (parseInput input) [1..1000]

stepThrough :: Vector2D Int -> [Int] -> IO ()
stepThrough _ [] = do
  putStrLn "Done."
stepThrough vector (step:steps) = do
  putStrLn $ "Step: " ++ show step
  let solutions = findPath (0, 0) vector step
  let plottedSolutions = (\(_, p) -> drawPathLines p vector) <$> take 10 solutions
  putStrLn $ unlines $ foldl1 plotSideways plottedSolutions
  threadDelay 100000
  stepThrough vector steps

parseInput :: String -> Vector2D Int
parseInput str = Vector2D.fromList $ map digitToInt <$> lines str

drawPath :: [Coord2D] -> Vector2D Int -> String
drawPath coords vector = unlines $ drawPathLines coords vector

drawPathLines :: [Coord2D] -> Vector2D Int -> [String]
drawPathLines coords vector = Vector2D.toList $ Vector2D.map toChar $ foldl (\vect c -> updateAt c (+10) vect) vector coords
  where
    toChar x = if x > 10 then intToDigit (x-10) else '.'

plotSideways :: [String] -> [String] -> [String]
plotSideways = zipWith (\a b -> a ++ " " ++ b)

findPath :: Coord2D -> Vector2D Int -> Int -> [(Int, [Coord2D])]
findPath startCoord vector = findPath' [(0, [startCoord])]
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

