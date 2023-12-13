module Main (main) where

import Data.List.Utils (split)
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ puzzleScore <$> parseInput input

type Puzzle = [String]

parseInput :: String -> [Puzzle]
parseInput input = lines <$> split "\n\n" input

puzzleScore :: Puzzle -> Int
puzzleScore xs = rowsScore*100 + colsScore
  where
    rowsScore = reflectionScore $ reflection xs
    colsScore = reflectionScore $ reflection $ transpose xs

reflectionScore :: Maybe (Puzzle, Puzzle) -> Int
reflectionScore (Just (xs, _)) = length xs
reflectionScore Nothing = 0

reflection :: Puzzle -> Maybe (Puzzle, Puzzle)
reflection xs = case filter isReflection (possibleReflections xs) of
  [ref] -> Just ref
  _ -> Nothing

isReflection :: (Puzzle, Puzzle) -> Bool
isReflection (xs, ys) = and $ zipWith (==) xs ys

possibleReflections :: Puzzle -> [(Puzzle, Puzzle)]
possibleReflections xs = map (`reflectionAt` xs) [1..(length xs - 1)]

reflectionAt :: Int -> Puzzle -> (Puzzle, Puzzle)
reflectionAt n xs = (reverse $ take n xs, drop n xs)
