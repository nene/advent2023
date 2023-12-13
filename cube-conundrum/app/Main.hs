module Main (main) where

import Data.Char (isDigit, isSpace)
import Data.List (find)
import Data.List.Utils (split)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ show (process input)

process :: String -> Int
process xs = sum $ powerRgb <$> maximumRgb <$> parsedGames
  where
    parsedGames = parseGame <$> lines xs

data Game = Game Int [Rgb]
data Rgb = Rgb Int Int Int

instance Show Game where
  show (Game n colors) = "Game " ++ (show n) ++ ": " ++ (show colors)

instance Show Rgb where
  show (Rgb r g b) = "R:" ++ show r ++ " G:" ++ show g ++ " B:" ++ show b

-- gameNr :: Game -> Int
-- gameNr (Game n _) = n

parseGame :: String -> Game
parseGame ('G':'a':'m':'e':' ':xs) = Game (read (takeWhile isDigit xs)) (parseGame' (dropWhile isDigit xs))
  where
    parseGame' (':':' ':xxs) = parseRgbList xxs
    parseGame' _ = error "incorrect Game'"
parseGame _ = error "incorrect Game"

parseRgbList :: String -> [Rgb]
parseRgbList xs = parseRgb <$> split "; " xs

parseRgb :: String -> Rgb
parseRgb xs = colorsToRgb $ parseColor <$> split ", " xs

colorsToRgb :: [(Int, String)] -> Rgb
colorsToRgb colors = Rgb (extract "red") (extract "green") (extract "blue")
  where
    extract color = case find (\(_, c) -> c == color) colors of
      Just (count, _) -> count
      Nothing -> 0

parseColor :: String -> (Int, String)
parseColor xs = (count, color)
  where
    count = read (takeWhile isDigit xs) :: Int
    color = dropWhile isSpace (dropWhile isDigit xs)

-- isPossibleGame :: Rgb -> Game -> Bool
-- isPossibleGame rgb (Game _ rgbs) = all (isPossibleRgb rgb) rgbs

-- isPossibleRgb :: Rgb -> Rgb -> Bool
-- isPossibleRgb (Rgb r g b) (Rgb r' g' b') = r' <= r && g' <= g && b' <= b

maximumRgb :: Game -> Rgb
maximumRgb (Game _ rgbs) = foldr maxRgb (Rgb 0 0 0) rgbs

maxRgb :: Rgb -> Rgb -> Rgb
maxRgb (Rgb r g b) (Rgb r' g' b') = Rgb (max r r') (max g g') (max b b')

powerRgb :: Rgb -> Int
powerRgb (Rgb r g b) = r * g * b
