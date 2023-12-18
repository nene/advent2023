module Vector2D (Vector2D, Coord2D, fromList, toList, at, setAt, updateAt, size, Vector2D.map, fill) where

import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as Vector

type Vector2D a = Vector (Vector a)

type Coord2D = (Int, Int)

-- 2D list to Vector2D
fromList :: [[a]] -> Vector2D a
fromList xs = Vector.fromList (Vector.fromList <$> xs)

-- Vector2D to 2D list
toList :: Vector2D a -> [[a]]
toList vector = Vector.toList <$> Vector.toList vector

-- value at coordinate
at :: Vector2D a -> Coord2D -> Maybe a
at rows (rowIndex, colIndex) = do
  row <- rows !? rowIndex
  row !? colIndex

-- Set value at coordinate
setAt :: Coord2D -> a -> Vector2D a -> Vector2D a
setAt (rowIndex, colIndex) value rows = rows // [(rowIndex, newRow)]
  where
    newRow = case rows !? rowIndex of
      Nothing -> Vector.empty
      Just row -> row // [(colIndex, value)]

-- Update value at coordinate
updateAt :: Coord2D -> (a -> a) -> Vector2D a -> Vector2D a
updateAt coord fn vector = case vector `at` coord of
  Nothing -> vector
  Just oldValue -> setAt coord (fn oldValue) vector

-- (height, width) of a 2D vector
size :: Vector2D a -> Coord2D
size rows = (length rows, maybe 0 length (rows !? 0))

-- a map over 2D vector
map :: (a -> b) -> Vector2D a -> Vector2D b
map f = Vector.map (\row -> Vector.map f row)

-- creates 2D vector filled with some value
fill :: Coord2D -> a -> Vector2D a
fill (rowCount, colCount) v = Vector.replicate rowCount $ Vector.replicate colCount v
