module Main (main) where

import Vector3D (Coord3D, Vector3D, at)
import qualified Vector3D
import qualified Vector2D
import Data.List.Utils (split, uniq)
import Data.Vector ((//))
import Data.List (transpose, find, sort)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let rawBricks = parseInput input
  let bricks = rawToSolidBrick <$> rawBricks
  let size = inputSize rawBricks
  let cube = initialCube size
  putStrLn $ visualizeCube $ addAllBricks bricks cube
  let (resultCube, resultBricks) = fallAllBricks bricks cube
  putStrLn $ visualizeCube resultCube
  putStrLn "Can be disintegrated:"
  print $ length $ filter not $ zipWith (\br i -> isSupportingBrick br i resultBricks resultCube) resultBricks [1..]

type RawBrick = (Coord3D, Coord3D)
type Brick = [Coord3D]

parseInput :: String -> [RawBrick]
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
    intToChar n = cycle ['A'..'Z'] !! (n-1)

    planes :: [[[Int]]]
    planes = reverse <$> transpose (Vector3D.toList vect)

inputSize :: [RawBrick] -> Coord3D
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

fallAllBricks :: [Brick] -> Vector3D Int -> (Vector3D Int, [Brick])
fallAllBricks bricks cube = (resultCube, reverse resultBricks)
  where
    (resultCube, resultBricks) = foldl fall (cube, []) $ zip bricks [1..]
    fall (oldCube, newBricks) (brick, i) = (newCube, newBrick : newBricks)
      where
        newCube = fillCoords newBrick i oldCube
        newBrick = fallBrick brick oldCube

fallBrick :: Brick -> Vector3D Int -> Brick
fallBrick brick cube = newBrick
  where
    newBrick = translateX (minNewX - minOldX) brick
    translateX dx = map (\(x,y,z) -> (x+dx,y,z))
    minNewX = maximum $ (`fallCoord` cube) <$> brick
    minOldX = minimum $ Vector3D.coord3DX <$> brick

fallCoord :: Coord3D -> Vector3D Int -> Int
fallCoord (x,y,z) cube = case find (\ x' -> cube `at` (x', y, z) /= Just 0) (reverse [0 .. x]) of
  Just newX -> newX + 1
  Nothing -> x

addAllBricks :: [Brick] -> Vector3D Int -> Vector3D Int
addAllBricks bricks cube = foldl (\c (b, i) -> fillCoords b i c) cube $ zip bricks [1..]

fillCoords :: [Coord3D] -> Int -> Vector3D Int -> Vector3D Int
fillCoords coords value cube = foldl (\acc c -> Vector3D.setAt c value acc) cube coords

rawToSolidBrick :: RawBrick -> Brick
rawToSolidBrick ((x,y,z), (x', y', z'))
  | x /= x' = [(x2,y,z) | x2 <- range x x']
  | y /= y' = [(x,y2,z) | y2 <- range y y']
  | z /= z' = [(x,y,z2) | z2 <- range z z']
  | otherwise = [(x, y, z)]

range :: Int -> Int -> [Int]
range a b
  | a < b = [a..b]
  | otherwise = reverse [b..a]

isSupportingBrick :: Brick -> Int -> [Brick] -> Vector3D Int -> Bool
isSupportingBrick brick value allBricks cube = any isOnlySupportedBy supportedBricks
  where
    isOnlySupportedBy brickA = [value] == supportersOf brickA
    supportersOf brickA = uniq $ sort $ filter (/=av) $ filter (/=0) $ (\c -> fromMaybe 0 $ cube `at` below c) <$> brickA
      where av = fromMaybe 0 $ cube `at` head brickA

    supportedBricks = (\v -> allBricks !! (v-1)) <$> supportedBrickValues
    supportedBrickValues = uniq $ sort $ map (fromMaybe 0 . (cube `at`)) supportedCoords
    supportedCoords = above <$> filter isSupportingCoord brick
    isSupportingCoord coord = supports coord cube /= value && supports coord cube /= 0

supports :: Coord3D -> Vector3D Int -> Int
supports coord cube = fromMaybe 0 (cube `at` above coord)

above :: Coord3D -> Coord3D
above (x,y,z) = (x+1, y, z)

below :: Coord3D -> Coord3D
below (x,y,z) = (x-1, y, z)
