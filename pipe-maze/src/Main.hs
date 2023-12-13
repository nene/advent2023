{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Main (main) where

import Data.Vector (fromList, Vector, (!), findIndex, elemIndex, imap, (//), toList)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ fromMaybe "" $ process $ parseInput input

showMaze :: Maze -> String
showMaze maze = unlines (toList $ toList <$> maze)

process :: Maze -> Maybe String
process maze = do 
  start <- startCoord maze
  return (showMaze $ drawPath maze $ mazePath start maze)

drawPath :: Maze -> [Coord] -> Maze
drawPath maze path = imap updateLine maze
  where
    updateLine y line = line // [(x, '#') | (Coord x y') <- path, y == y']

mazePath :: Coord -> Maze -> [Coord]
mazePath start maze = fst <$> fullPath
  where
    fullPath = step0 : walkCircle maze step1
    step1 = walk maze step0
    step0 = (start, startDir)
    startDir = head $ dirsAtStart start maze

type Maze = Vector (Vector Char)

parseInput :: String -> Maze
parseInput str = fromList (fromList <$> lines str)

startCoord :: Maze -> Maybe Coord
startCoord maze = do
  y <- findIndex (elem 'S') maze
  x <- elemIndex 'S' (maze ! y)
  return (Coord x y)

data Coord = Coord Int Int

instance Show Coord where
  show :: Coord -> String
  show (Coord x y) = "{"++show x ++","++ show y++"}"

instance Num Coord where
  (+) :: Coord -> Coord -> Coord
  Coord x y + Coord x' y' = Coord (x+x') (y+y')

data Dir = N | S | E | W deriving (Eq, Show)

dirs :: Char -> [Dir]
dirs '|' = [N, S]
dirs '-' = [E, W]
dirs 'L' = [N, E]
dirs 'J' = [N, W]
dirs '7' = [S, W]
dirs 'F' = [S, E]
dirs '.' = []
dirs _ = [N, S, E, W]

opposite :: Dir -> Dir
opposite N = S
opposite S = N
opposite E = W
opposite W = E

dirsAtStart :: Coord -> Maze -> [Dir]
dirsAtStart coord maze = filter isOpen [N, S, E, W]
  where
    isOpen dir = elem (opposite dir) $ dirs (maze `at` (coord `atDir` dir))

at :: Maze -> Coord -> Char
at maze (Coord x y) = (maze ! y) ! x

atDir :: Coord -> Dir -> Coord
atDir (Coord x y) N = Coord x (y-1)
atDir (Coord x y) S = Coord x (y+1)
atDir (Coord x y) E = Coord (x+1) y
atDir (Coord x y) W = Coord (x-1) y

walkCircle :: Maze -> (Coord, Dir) -> [(Coord, Dir)]
walkCircle maze coordDir = takeWhile (not . isStart) $ walkAll maze coordDir
  where isStart (coord, _) = maze `at` coord == 'S'

walkAll :: Maze -> (Coord, Dir) -> [(Coord, Dir)]
walkAll maze = iterate (walk maze)

walk :: Maze -> (Coord, Dir) -> (Coord, Dir)
walk maze (coord, dir) = (coord2, dir2)
  where
    dir2 = head $ filter (/= opposite dir) $ dirs $ maze `at` coord2
    coord2 = coord `atDir` dir
