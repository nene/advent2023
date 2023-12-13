{-# LANGUAGE InstanceSigs #-}
module Main (main) where

import Data.List.Utils (split)
import qualified Data.Map as Map
import Data.Map.Internal ((!), keys)
-- import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "input.txt"
  process $ parseInput input

type Graph = Map.Map String (String, String)

parseInput :: String -> (String, Graph)
parseInput str = case split "\n\n" str of
  [lr,xs] -> (lr, Map.fromList $ parseNode <$> lines xs)
  _ -> error "Invalid input"

parseNode :: String -> (String, (String, String))
parseNode [a, b, c, ' ', '=', ' ', '(', d, e, f, ',', ' ', g, h, i, ')'] = ([a,b,c], ([d,e,f], [g,h,i]))
parseNode _ = error "Invalid Node"

isFirstStep :: String -> Bool
isFirstStep [_,_,'A'] = True
isFirstStep _ = False

isFinalStep :: String -> Bool
isFinalStep [_,_,'Z'] = True
isFinalStep _ = False

process :: (String, Graph) -> IO ()
process (lrs, graph) = do
  print starts
  print $ head $ dropWhile divisibleToOtherSizes size1multiples
  where
    size1multiples = [size1 * x | x <- [1..]]
    size1 = head sizes
    sizes = map (\s -> walkPattern s lrs graph) starts
    starts = filter isFirstStep $ keys graph
    divisibleToOtherSizes x = not $ all (\s -> x `mod` s == 0) (take 5 (tail sizes))



walkPattern :: String -> String -> Graph -> Integer
walkPattern start lrs graph = toInteger $ length $ takeWhile isNonFinalStep allSteps
  where
    isNonFinalStep s = not $ isFinalStep s
    allSteps = walk start (cycle lrs) graph

walk :: String -> String -> Graph -> [String]
walk step (lr:xs) graph = step : walk (leftOrRight (graph ! step)) xs graph
  where
    leftOrRight = case lr of
      'L' -> fst
      'R' -> snd
      _ -> error "Expected L or R"
walk _ _ _ = error "Invalid graph"
