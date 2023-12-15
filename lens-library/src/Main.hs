module Main (main) where

import Data.Char (ord, isAlpha)
import Data.List.Utils (split)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (findIndex)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ totalFocusPower $ process $ parseInput input

hash :: String -> Int
hash = foldl op 0
  where
    op acc c = (acc + ord c) * 17 `mod` 256

type Lens = (String, Int)

parseInput :: String -> [Lens]
parseInput input = parseStep <$> split "," input
  where
    parseStep :: String -> Lens
    parseStep xs = case span isAlpha xs of
      (name, rest) -> (name, parseCmd rest)

    parseCmd :: String -> Int
    parseCmd "-" = 0
    parseCmd ('=':xs) = read xs
    parseCmd c = error ("Unexpected command: " ++ c)

process :: [Lens] -> IntMap [Lens]
process = foldl step IntMap.empty

step :: IntMap [Lens] -> Lens -> IntMap [Lens]
step boxmap (name, 0) = IntMap.update removeLens (hash name) boxmap
  where
    removeLens :: [Lens] -> Maybe [Lens]
    removeLens lenses = case filter ((/=name) . fst) lenses of
      [] -> Nothing
      xs -> Just xs
step boxmap (name, focus) = IntMap.alter addLens (hash name) boxmap
  where
    addLens :: Maybe [Lens] -> Maybe [Lens]
    addLens Nothing = Just [(name, focus)]
    addLens (Just lenses) = case findIndex ((==name) . fst) lenses of
      Just index -> Just (setAt index (name, focus) lenses)
      Nothing -> Just ((name, focus) : lenses)

setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 el (_:xs) = el:xs
setAt n el (x:xs) = x : setAt (n-1) el xs

totalFocusPower :: IntMap [Lens] -> Int
totalFocusPower boxmap = sum $ (\(i, lenses) -> boxFocusPower (i+1) lenses ) <$> IntMap.toList boxmap

boxFocusPower :: Int -> [Lens] -> Int
boxFocusPower boxNr lenses = sum $ zipWith (\(_, focLen) slotNr -> boxNr * slotNr * focLen) (reverse lenses) [1..]
