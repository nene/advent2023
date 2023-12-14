module Main (main) where
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ score $ tiltNorth $ lines input

tiltNorth :: [String] -> [String]
tiltNorth xs = turnLeft $ tiltLine <$> turnRight xs
  where
    turnLeft = reverse . transpose
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
