module Vector3D (
  Vector3D,
  Coord3D,
  fromList,
  toList,
  at,
  setAt,
  updateAt,
  size,
  Vector3D.map,
  findCoord,
  fill,
  coord3DX,
  coord3DY,
  coord3DZ
) where

import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as Vector
import qualified Vector2D
import Data.Maybe (isJust)

type Vector3D a = Vector (Vector (Vector a))

type Coord3D = (Int, Int, Int)

-- 3D list to Vector3D
fromList :: [[[a]]] -> Vector3D a
fromList xs = Vector.fromList (Vector2D.fromList <$> xs)

-- Vector3D to 3D list
toList :: Vector3D a -> [[[a]]]
toList vector = Vector2D.toList <$> Vector.toList vector

at :: Vector3D a -> Coord3D -> Maybe a
at xs (xIndex, yIndex, zIndex) = do
  x <- xs !? xIndex
  y <- x !? yIndex
  y !? zIndex

-- Set value at coordinate
setAt :: Coord3D -> a -> Vector3D a -> Vector3D a
setAt (xIndex, yIndex, zIndex) value xs = xs // [(xIndex, newX)]
  where
    newX = case xs !? xIndex of
      Nothing -> Vector.empty
      Just ys -> Vector2D.setAt (yIndex, zIndex) value ys

-- Update value at coordinate
updateAt :: Coord3D -> (a -> a) -> Vector3D a -> Vector3D a
updateAt coord fn vector = case vector `at` coord of
  Nothing -> vector
  Just oldValue -> setAt coord (fn oldValue) vector

-- (sizeX, sizeY, sizeZ) of a 2D vector
size :: Vector3D a -> Coord3D
size xs = (length xs, sizeY, sizeZ)
  where
    (sizeY, sizeZ) = maybe (0, 0) Vector2D.size (xs !? 0)

-- a map over 3D vector
map :: (a -> b) -> Vector3D a -> Vector3D b
map f = Vector.map (Vector.map (Vector.map f))

-- searches for value from 3D vector and returns a coordinate
findCoord :: (a -> Bool) -> Vector3D a -> Maybe Coord3D
findCoord fn vect = do
  xIndex <- Vector.findIndex (isJust . Vector2D.findCoord fn) vect
  ys <- vect !? xIndex
  (yIndex, zIndex) <- Vector2D.findCoord fn ys
  return (xIndex, yIndex, zIndex)

-- creates 3D vector filled with some value
fill :: Coord3D -> a -> Vector3D a
fill (xCount, yCount, zCount) v = Vector.replicate xCount $ Vector2D.fill (yCount, zCount) v

coord3DX :: Coord3D -> Int
coord3DX (x, _, _) = x

coord3DY :: Coord3D -> Int
coord3DY (_, y, _) = y

coord3DZ :: Coord3D -> Int
coord3DZ (_, _, z) = z
