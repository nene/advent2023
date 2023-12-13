module Main (main) where

import Data.Char (isDigit)
import Relude.List (maybeAt)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ show (process input)

process :: String -> Int
process xs = sum validGearRatios
  where
    validGearRatios = gearRatio <$> partsOfGear <$> validGears
    validGears = filter isValidGear gears
    isValidGear g = (length $ partsOfGear g) == 2
    partsOfGear g = filter (\nr -> g `isAdjecentToNumber` nr) parts
    gears = gearsInMatrix $ lines xs
    parts = numbersInMatrix $ lines xs

data Pos = Pos Int Int

instance Show Pos where
  show (Pos x y) = "{" ++ show x ++ "," ++ show y ++ "}"

instance Eq Pos where
  (Pos x y) == (Pos x' y') = x == x' && y == y'

gearRatio :: [(String, Pos)] -> Int
gearRatio ((p1, _):(p2, _):_) = read p1 * read p2
gearRatio _ = error "Unexpected nr of parts"

numbersInMatrix :: [String] -> [(String, Pos)]
numbersInMatrix = itemsInMatrix numbersInLine

gearsInMatrix :: [String] -> [(String, Pos)]
gearsInMatrix = itemsInMatrix gearsInLine

itemsInMatrix :: (String -> [(String, Int)]) -> [String] -> [(String, Pos)]
itemsInMatrix fn xs = concat $ imap toMatrixCoord (fn <$> xs)
  where
    toMatrixCoord y pairs = toMatrixCoord' y <$> pairs
    toMatrixCoord' y (nr, x) = (nr, Pos x y)

numbersInLine :: String -> [(String, Int)]
numbersInLine str = numbersInLine' str 0
  where
    numbersInLine' "" _ = []
    numbersInLine' (x:xs) n | isDigit x = (takeDigit (x:xs), n) : numbersInLine' (dropDigit (x:xs)) (n + length (takeDigit (x:xs)))
    numbersInLine' (_:xs) n = numbersInLine' xs (n + 1)
    takeDigit = takeWhile isDigit
    dropDigit = dropWhile isDigit

gearsInLine :: String -> [(String, Int)]
gearsInLine str = gearsInLine' str 0
  where
    gearsInLine' "" _ = []
    gearsInLine' (x:xs) n | isGear x = ("*", n) : gearsInLine' xs (n + 1)
    gearsInLine' (_:xs) n = gearsInLine' xs (n + 1)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && not (isDot c)

isDot :: Char -> Bool
isDot c = c == '.'

isGear :: Char -> Bool
isGear c = c == '*'

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn list = imap' fn 0 list
  where
    imap' _ _ [] = []
    imap' f i (x:xs) = (f i x) : imap' f (i+1) xs

isAdjecentToNumber :: (String, Pos) -> (String, Pos) -> Bool
isAdjecentToNumber (_, pos) number = any (== pos) $ aroundPositions number

isAdjecentToSymbol :: [String] -> (String, Pos) -> Bool
isAdjecentToSymbol matrix nr = any isSymbol $ atMatrix matrix <$> aroundPositions nr

atMatrix :: [String] -> Pos -> Char
atMatrix matrix (Pos x y) = case maybeAt y matrix of
  Nothing -> '.'
  Just line -> case maybeAt x line of
    Nothing -> '.'
    Just c -> c

aroundPositions :: (String, Pos) -> [Pos]
aroundPositions (nr, Pos x y) =
  [Pos x' (y-1) | x' <- [startX..endX]] ++
  [Pos startX y] ++
  [Pos endX y] ++
  [Pos x' (y+1) | x' <- [startX..endX]]
  where
    startX = x - 1
    endX = x + length nr
