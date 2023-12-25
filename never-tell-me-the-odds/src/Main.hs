module Main (main) where

import System.Environment (getArgs)
import Data.List.Utils (split)

type Point2D = (Double, Double)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let pairs = parseInput input
  print pairs
  let (p, v) = head pairs
  print $ deriveLineEq p v
  print $ uncurry deriveLineEq <$> pairs
  print $ intersect (head pairs) (pairs !! 1)

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

-- A: px:19, py:13 @ vx:-2, vy:1
-- B: px:18, py:19 @ vx:-1, vy:-1

-- x1:19 y1:13
-- x2:17 y2:14

--     y2 - y1   14 - 13    1
-- m = ------- = ------- = ---
--     x2 - x1   17 - 19   -2
--
-- y = mx + b = (-0.5)x + b
-- (x1: 19, y1:13)
-- y1 = (-0.5)x1 + b = -0.5*19 + b = -9.5 + b
-- 13 = -9.5 + b
-- 13 + 9.5 = b
-- b = 22.5

-- y = mx + b
-- y = (-0.5)x + 22.5


--      vy
-- m = ----
--      vx
--
-- y1 = m * x1 + b
-- y1 - m * x1 = b
-- b = y1 - m*x1
--
-- y = mx + b

deriveLineEq :: Point2D -> Point2D -> String
deriveLineEq (x1, y1) (vx, vy) = show m ++ "*x + " ++ show b
  where
    b = y1 - m * x1
    m = vy / vx

deriveLineY :: Point2D -> Point2D -> Double -> Double
deriveLineY (x1, y1) (vx, vy) x = m * x + b
  where
    b = y1 - m * x1
    m = vy / vx

-- m1*x + b1 = m2*x + b2
-- m1*x - m2*x = b2 - b1
-- (m1-m2)*x = b2 - b1
-- x = (b2 - b1) / (m1 - m2)

intersect :: (Point2D, Point2D) -> (Point2D, Point2D) -> Point2D
intersect ((x1,y1), (vx1,vy1)) ((x2,y2), (vx2,vy2)) = (x, y)
  where
    y = m1 * x + b1
    x = (b2 - b1) / (m1 - m2)
    b1 = y1 - m1 * x1
    m1 = vy1 / vx1
    b2 = y2 - m2 * x2
    m2 = vy2 / vx2



-- |
--20                  
-- |                 X
-- |                / 
-- |               /  
-- |            \ /   
--15             /\   
-- |                \ 
-- |                  X 
-- |                    
-- |
--10
-- |
-- |
-- |
-- |
-- 5
-- |
-- |
-- |
-- |
-- 0----5----10---15---20 

-- -2x + 19 = 1y + 13
-- 19 - 2x - 13 = y
-- 6 - 2x = y

-- y = 13
-- x = 19
-- x17 y14
-- x15 y15
-- x13 y16
-- x11 y17
-- x9 y18
-- x5 y20
-- x1 y22
-- x0 y22.5

-- y = 22.5 - 0.5x
-- 2y = 45 - x

-- B: px:18, py:19 @ vx:-1, vy:-1
--                   vx:1,  vy:1
--
-- y = 1x + 1

-- x + 1 = 22.5 - 0.5x | *2
-- 2x + 2 = 45 - x
-- 2x + x = 45 - 2
-- 3x = 43
-- x = 14.333
-- y = 14.333 + 1
-- y = 15.333
