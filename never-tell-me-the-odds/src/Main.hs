module Main (main) where
import System.Environment (getArgs)

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  print input
