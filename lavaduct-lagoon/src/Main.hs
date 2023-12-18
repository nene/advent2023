module Main (main) where

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ parseInput input

data Dir = L | R | U | D deriving (Eq, Show)

parseInput :: String -> [(Dir, Int)]
parseInput txt = parseLine <$> lines txt
  where
    parseLine s = case words s of
      ["L",i,_] -> (L, read i)
      ["R",i,_] -> (R, read i)
      ["U",i,_] -> (U, read i)
      ["D",i,_] -> (D, read i)
      _ -> error ("Invalid input: " ++ s)
