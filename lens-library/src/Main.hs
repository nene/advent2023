module Main (main) where

import Data.Char (ord)
import Data.List.Utils (split)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ hash <$> split "," input

hash :: String -> Int
hash = foldl op 0
  where
    op acc c = (acc + ord c) * 17 `mod` 256
