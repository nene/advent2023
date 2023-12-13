module Main (main) where

import Data.List.Utils (split, join)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ validArrangements $ expand $ head $ drop 1 $ fromMaybe [] $ parseInput input
  -- print $ sum (length . validArrangements . expand <$> take 2 (parseInput input))

type Record = (String, [Int])

parseInput :: String -> Maybe [Record]
parseInput str = parseRecord `traverse` lines str
  where
    parseRecord s = case words s of
      [a,b] -> Just (a, parseNumbers b)
      _ -> Nothing
    parseNumbers s = read <$> split "," s

validArrangements :: Record -> [String]
validArrangements (str, counts) = filter isValid (arrangements str)
  where isValid xs = countHashes xs == counts

countHashes :: String -> [Int]
countHashes str = filter (/=0) $ length <$> split "." str

arrangements :: String -> [String]
arrangements [] = []
arrangements ['?'] = [".","#"]
arrangements [c] = [[c]]
arrangements ('?':xs) = [y:ys | y <- ['.','#'], ys <- arrangements xs]
arrangements (x:xs) = [x:ys | ys <- arrangements xs]

expand :: Record -> Record
expand (str, counts) = (join "?" (replicate 5 str), concat (replicate 5 counts))
