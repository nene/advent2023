{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- print $ sum $ uncurry distance <$> coordPairs (findGalaxies $ expand $ parseInput input)
  -- print $ process $ parseInput input
  print $ process $ parseInput input

process :: SkyMap -> Int
process skymap = sum $ uncurry distance <$> coordPairs expanded
  where
    expanded = foldr expandY expandedXs (expandableRows skymap)
    expandedXs = foldr expandX galaxies (expandableCols skymap)
    galaxies = findGalaxies skymap

type SkyMap = [String]

parseInput :: String -> SkyMap
parseInput = lines

showSkyMap :: SkyMap -> String
showSkyMap = unlines

data Coord = Coord Int Int

instance Show Coord where
  show :: Coord -> String
  show (Coord x y) = "{"++show x ++","++ show y++"}"

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x y) == (Coord x' y') = x == x' && y == y'

expandY :: Int -> [Coord] -> [Coord]
expandY y coords = (`expandWhenGreater` y) <$> coords
  where
    expandWhenGreater (Coord x y) y'
      | y > y' = Coord x (y+expFactor-1)
      | otherwise = Coord x y

expandX :: Int -> [Coord] -> [Coord]
expandX x coords = (`expandWhenGreater` x) <$> coords
  where
    expandWhenGreater (Coord x y) x'
      | x > x' = Coord (x+expFactor-1) y
      | otherwise = Coord x y

expFactor :: Int
expFactor = 1000000

expandableCols :: SkyMap -> [Int]
expandableCols = expandableRows . transpose

expandableRows :: SkyMap -> [Int]
expandableRows rows = snd <$> filter (\(row, _) -> '#' `notElem` row) (zip rows [0..])

findGalaxies :: SkyMap -> [Coord]
findGalaxies rows = concatMap galaxyCoords $ zip rows [0..]
  where
    galaxyCoords :: (String, Int) -> [Coord]
    galaxyCoords (row, y) = (`Coord` y) <$> galaxiesInRow row
    galaxiesInRow :: String -> [Int]
    galaxiesInRow xs = map snd $ filter (\(a, _) -> a == '#') (zip xs [0..])

coordPairs :: [Coord] -> [(Coord, Coord)]
coordPairs [] = []
coordPairs (c:ds) = [(c, d)| d <- ds] ++ coordPairs ds

distance :: Coord -> Coord -> Int
distance (Coord x y) (Coord x' y') = abs (x - x') + abs (y - y')
