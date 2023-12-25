module Main (main) where

import System.Environment (getArgs)
import Data.List.Utils (split)

type Point2D = (Double, Double)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let paths = parseInput input
  print $ [a `intersectInFuture` b | (a,b) <- pairs paths]

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

intersectInFuture :: (Point2D, Point2D) -> (Point2D, Point2D) -> Maybe Point2D
intersectInFuture path1 path2 = do
  point <- maybeIntersect path1 path2
  return point
  -- if isInFuture point path1 && isInFuture point path2 then Just point else Nothing

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
