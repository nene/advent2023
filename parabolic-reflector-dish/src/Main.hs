module Main (main) where

import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let scores = map score $ iterate tiltCycle $ lines input
  let total = 1000000000
  let pattern = repeatingPattern $ take 100 $ drop 1000 scores
  let patternSize = length pattern
  print $ pattern !! ((total - 1000) `mod` patternSize)

tiltCycle :: [String] -> [String]
tiltCycle xs = turnRightAndTilt $ turnRightAndTilt $ turnRightAndTilt $ turnRightAndTilt xs
  where
    turnRightAndTilt ys = tiltLine <$> turnRight ys
    turnRight = transpose . reverse

tiltLine :: String -> String
tiltLine = tiltLine' []
  where
    tiltLine' carry "" = carry
    tiltLine' carry ('O':xs) = tiltLine' ('O':carry) xs
    tiltLine' carry ('.':xs) = '.' : tiltLine' carry xs
    tiltLine' carry ('#':xs) = carry ++ "#" ++ tiltLine' [] xs
    tiltLine' _ (c:_) = error ("Unexpected symbol: " ++ [c])

score :: [String] -> Int
score xs = sum $ zipWith scoreLine [1..] (reverse xs)

scoreLine :: Int -> String -> Int
scoreLine v xs = length (filter (=='O') xs) * v

repeatingPattern :: Eq a => [a] -> [a]
repeatingPattern xs = head $ filter isRepeating $ map (`take` xs) [1..]
  where
    isRepeating ys = and $ zipWith (==) xs (cycle ys)
