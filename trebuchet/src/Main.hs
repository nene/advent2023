module Main (main) where

import Data.Char (isDigit)
import Data.List (findIndex)
import Data.List.Utils (startswith)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ show (process input)

process :: String -> Integer
process input = sum (calibrationValue <$> (lines input))

calibrationValue :: String -> Integer
calibrationValue s = read [head (digits s), last (digits s)]

digits :: String -> String
digits "" = ""
digits (x:xs) | isDigit x = x : (digits xs)
digits (x:xs) = case (takeDigitWord (x:xs)) of
  (Just digit) -> head (show (digit + 1)) : (digits xs)
  Nothing -> digits xs

takeDigitWord :: String -> Maybe Int
takeDigitWord xs = findIndex (\digit -> startswith digit xs) digitWords

digitWords :: [String]
digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
