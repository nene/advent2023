module Main (main) where

main :: IO ()
main = do
  print $ product counts
  where
    counts = winWaysCount <$> races
    races = [(61709066, 643118413621041)]
    -- races = [(71530, 940200)]

winWaysCount :: (Int, Int) -> Int
winWaysCount (time, winDist) = length $ winningRaces time winDist

winningRaces :: Int -> Int -> [Int]
winningRaces time winDist = filter (> winDist) (possibleRaces time)

possibleRaces :: Int -> [Int]
possibleRaces time = [race speed time | speed <- [1..time]]

race :: Int -> Int -> Int
race speed time = speed * (time - speed)
