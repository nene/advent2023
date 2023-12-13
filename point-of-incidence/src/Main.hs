module Main (main) where

import Data.List.Utils (split, uniq)
import Data.List (transpose, find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let puzzles = parseInput input
  print $ newPuzzleScore2 <$> (take 10 $ drop 20 puzzles)

type Puzzle = [String]

parseInput :: String -> [Puzzle]
parseInput input = lines <$> split "\n\n" input

newPuzzleScore :: Puzzle -> Int
newPuzzleScore pz = snd $ newPuzzleScore2 pz

newPuzzleScore2 :: Puzzle -> ((Int, [Int], Int, [Int]), Int)
newPuzzleScore2 pz = (input, result)
  where
    result = case input of
      (_, [], _, []) -> 0
      (_, [r], _, []) -> r
      (r', rs, _, []) -> head $ filter (/= r') rs
      (_, [], _, [c]) -> c
      (_, [], c', cs) -> head $ filter (/= c') cs
      (r', [r], c', [c]) -> if r == r' && c /= 0 then c else r
      (r', rs, c', cs) -> case (filter (/=r') rs, filter (/=c') cs) of
        ((r:rs), _) -> r
        (_, (c:cs)) -> c
    input = (oldRow * 100, (*100) <$> newRows, oldCol, newCols)
    newRows = uniq $ filter (>0) $ rowScore <$> smudge pz
    newCols = uniq $ filter (>0) $ colScore <$> smudge pz
    oldRow = rowScore pz
    oldCol = colScore pz

oldPuzzleScore :: Puzzle -> Int
oldPuzzleScore pz = rowScore pz * 100 + colScore pz

rowScore :: Puzzle -> Int
rowScore = reflectionScore . reflection

colScore :: Puzzle -> Int
colScore = reflectionScore . reflection . transpose

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

smudge :: Puzzle -> [Puzzle]
smudge pz = [chunksOf lineLen (invertAt n linePz) | n <- [0..(length linePz - 1)]]
  where
    lineLen = length (head pz)
    linePz = concat pz

invertAt :: Int -> String -> String
invertAt _ "" = ""
invertAt 0 ('.':xs) = '#' : xs
invertAt 0 ('#':xs) = '.' : xs
invertAt n (x:xs) = x : invertAt (n-1) xs

