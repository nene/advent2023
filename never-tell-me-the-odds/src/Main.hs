module Main (main) where

import System.Environment (getArgs)
import Data.List.Utils (split)
import Data.Maybe (isJust)

type Point2D = (Double, Double)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let paths = parseInput input
  let area = ((200000000000000, 200000000000000), (400000000000000, 400000000000000))
  let intersections = [validIntersect area a b | (a,b) <- pairs paths]
  print intersections
  print $ length $ filter isJust intersections

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x,y)| y <- xs] ++ pairs xs

-- 19, 13, 30 @ -2,  1, -2
parseInput :: String -> [(Point2D, Point2D)]
parseInput input = parseLine <$> lines input
  where
    parseLine ln = case split " @ " ln of
      [p, v] -> (parseCoord p, parseCoord v)
      _ -> error "Invalid input"

    parseCoord txt = case split ", " txt of
      [x,y,_z] -> (read x, read y)
      _ -> error "Invalid input"

validIntersect :: (Point2D, Point2D) -> (Point2D, Point2D) -> (Point2D, Point2D) -> Maybe Point2D
validIntersect area path1 path2 = do
  point <- maybeIntersect path1 path2
  if isInFuture point path1 && isInFuture point path2 && isInArea area point
  then 
    Just point
  else
    Nothing

isInFuture :: Point2D -> (Point2D, Point2D) -> Bool
isInFuture (x,_y) ((x1, _y1), (vx, _vy))
  | vx > 0 = x > x1
  | otherwise = x <= x1

isInArea :: (Point2D, Point2D) -> Point2D -> Bool
isInArea ((x1,y1), (x2,y2)) (x, y) = x2 >= x && x >= x1 && y2 >= y && y >= y1

maybeIntersect :: (Point2D, Point2D) -> (Point2D, Point2D) -> Maybe Point2D
maybeIntersect path1 path2 = if isInfinite x || isInfinite y then Nothing else Just (x, y)
  where
    (x, y) = path1 `intersect` path2

intersect :: (Point2D, Point2D) -> (Point2D, Point2D) -> Point2D
intersect ((x1,y1), (vx1,vy1)) ((x2,y2), (vx2,vy2)) = (x, y)
  where
    y = m1 * x + b1
    x = (b2 - b1) / (m1 - m2)
    b1 = y1 - m1 * x1
    m1 = vy1 / vx1
    b2 = y2 - m2 * x2
    m2 = vy2 / vx2
