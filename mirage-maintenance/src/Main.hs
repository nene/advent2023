module Main (main) where

import Data.List.Utils (split)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ predictFirst . diffsTillZero <$> parseInput input

parseInput :: String -> [[Int]]
parseInput str = map read . split " " <$> lines str

diffsTillZero :: [Int] -> [[Int]]
diffsTillZero xs = takeWhile (not . all (== 0)) $ diffs xs

diffs :: [Int] -> [[Int]]
diffs xs = xs : diffs (diff xs)

diff :: [Int] -> [Int]
diff = mapAdjecent (flip (-))

mapAdjecent :: (a -> a -> b) -> [a] -> [b]
mapAdjecent _ [] = []
mapAdjecent _ [_] = []
mapAdjecent fn (x:y:xs) = fn x y : mapAdjecent fn (y:xs)

predictLast :: [[Int]] -> Int
predictLast = foldr ((+) . last) 0

predictFirst :: [[Int]] -> Int
predictFirst = foldr ((-) . head) 0
