module Main (main) where
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!?), (//))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let mirrorPlan = parseInput input
  print $ foldl max 0 $ (`beamEnergy` mirrorPlan) <$> possibleDirections mirrorPlan

type MirrorPlan = Vector (Vector Char)

type BeamPlan = Vector (Vector [Dir])

type Coord = (Int, Int)

data Dir = U | D | L | R deriving (Show, Eq)

parseInput :: String -> MirrorPlan
parseInput input = Vector.fromList (Vector.fromList <$> lines input)

emptyBeamPlan :: MirrorPlan -> BeamPlan
emptyBeamPlan plan = (\row -> Vector.replicate (length row) []) <$> plan

toListFrom2DVector :: Vector (Vector a) -> [[a]]
toListFrom2DVector vector = Vector.toList <$> Vector.toList vector

showBeamPlan :: BeamPlan -> String
showBeamPlan plan = unlines (map showDirList <$> toListFrom2DVector plan)
  where
    showDirList [] = '.'
    showDirList [U] = '^'
    showDirList [D] = 'v'
    showDirList [R] = '>'
    showDirList [L] = '<'
    showDirList _ = '#'

possibleDirections :: MirrorPlan -> [(Coord, Dir)]
possibleDirections plan = fromLeft ++ fromRight ++ fromTop ++ fromBottom
  where
    fromLeft = [((row, 0), R) | row <- [0 .. lastRow]]
    fromRight = [((row, lastCol), L) | row <- [0 .. lastRow]]
    fromTop = [((0, col), D) | col <- [0 .. lastCol]]
    fromBottom = [((lastRow, col), U) | col <- [0 .. lastCol]]
    lastRow = rowCount plan - 1
    lastCol = colCount plan - 1

beamEnergy :: (Coord, Dir) -> MirrorPlan -> Int
beamEnergy (coord, dir) mirrorPlan =
  energizedTileCount $ last $ propagate coord dir mirrorPlan (emptyBeamPlan mirrorPlan)

energizedTileCount :: BeamPlan -> Int
energizedTileCount rows = sum $ sum . map energyNr <$> toListFrom2DVector rows
  where
    energyNr [] = 0
    energyNr _ = 1

-- Look up coordinate from 2D vector
at :: Vector (Vector a) -> Coord -> Maybe a
at rows (rowIndex, colIndex) = do
  row <- rows !? rowIndex
  row !? colIndex

colCount :: Vector (Vector a) -> Int
colCount rows = maybe 0 length (rows !? 0)

rowCount :: Vector (Vector a) -> Int
rowCount = length

-- Set value at coordinate in 2D vector
setAt :: Coord -> a -> Vector (Vector a) -> Vector (Vector a)
setAt (rowIndex, colIndex) value rows = rows // [(rowIndex, newRow)]
  where
    newRow = case rows !? rowIndex of
      Nothing -> Vector.empty
      Just row -> row // [(colIndex, value)]

-- Update value at coordinate in 2D vector
updateAt :: Coord -> (a -> a) -> Vector (Vector a) -> Vector (Vector a)
updateAt coord fn vector = case vector `at` coord of
  Nothing -> vector
  Just oldValue -> setAt coord (fn oldValue) vector

atDir :: Coord -> Dir -> Coord
atDir (row, col) U = (row-1, col)
atDir (row, col) D = (row+1, col)
atDir (row, col) L = (row, col-1)
atDir (row, col) R = (row, col+1)

propagate :: Coord -> Dir -> MirrorPlan -> BeamPlan -> [BeamPlan]
propagate coord dir mplan bplan =
  if hasBeam coord dir bplan
  then []
  else propagate' coord dir mplan (addBeam coord dir bplan)

-- with beam already added to BeamPlan
propagate' :: Coord -> Dir -> MirrorPlan -> BeamPlan -> [BeamPlan]
propagate' coord dir mplan bplan =
    case mplan `at` coord of
      Nothing -> []
      Just '.' -> bplan : propagate (coord `atDir` dir) dir mplan bplan
      Just '/' -> propagateReflect reflectLU coord dir mplan bplan
      Just '\\' -> propagateReflect reflectLD coord dir mplan bplan
      Just '|' -> propagateSplit splitV coord dir mplan bplan
      Just '-' -> propagateSplit splitH coord dir mplan bplan
      Just c -> error ("unexpected symbol " ++ [c])

propagateReflect :: (Dir -> Dir) -> Coord -> Dir -> MirrorPlan -> BeamPlan -> [BeamPlan]
propagateReflect reflectFn coord dir mplan bplan =
  bplan : propagate (coord `atDir` reflectFn dir) (reflectFn dir) mplan bplan

propagateSplit :: (Dir -> Maybe (Dir, Dir)) -> Coord -> Dir -> MirrorPlan -> BeamPlan -> [BeamPlan]
propagateSplit splitFn coord dir mplan bplan = case splitFn dir of
  Nothing -> bplan : propagate (coord `atDir` dir) dir mplan bplan
  Just (d1,d2) -> newBPlans1 ++ newBPlans2
    where
      newBPlans1 = bplan : propagate (coord `atDir` d1) d1 mplan bplan
      newBPlans2 = propagate (coord `atDir` d2) d2 mplan (last newBPlans1)

hasBeam :: Coord -> Dir -> BeamPlan -> Bool
hasBeam coord dir plan = case plan `at` coord of
  Nothing -> False
  Just dirs -> dir `elem` dirs

addBeam :: Coord -> Dir -> BeamPlan -> BeamPlan
addBeam coord dir = updateAt coord (dir:)

-- Mirror: '/'
reflectLU :: Dir -> Dir
reflectLU U = R
reflectLU D = L
reflectLU R = U
reflectLU L = D

-- Mirror: '\'
reflectLD :: Dir -> Dir
reflectLD U = L
reflectLD D = R
reflectLD R = D
reflectLD L = U

-- Splitter: '-'
splitH :: Dir -> Maybe (Dir, Dir)
splitH U = Just (L, R)
splitH D = Just (L, R)
splitH _ = Nothing

-- Splitter: '|'
splitV :: Dir -> Maybe (Dir, Dir)
splitV L = Just (U, D)
splitV R = Just (U, D)
splitV _ = Nothing
