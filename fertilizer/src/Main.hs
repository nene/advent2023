module Main (main) where

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ process $ safeParse input

data SeedFile = SeedFile [Int] [SeedMap]

data SeedMap = SeedMap String [Mapping]

data Mapping = Mapping Int Int Int

instance Show SeedFile where
  show (SeedFile seeds maps) = "Seeds: " ++ show seeds ++ ", maps: " ++ show maps

instance Show SeedMap where
  show (SeedMap name mappings) = name ++ ": " ++ show mappings

instance Show Mapping where
  show (Mapping a b c) = show a ++ " " ++ show b ++ " " ++ show c

safeParse :: String -> SeedFile
safeParse xs = case parse seedFile "(unknown)" xs of
  Left _ -> SeedFile [] []
  Right result -> result

seedFile :: GenParser Char st SeedFile
seedFile = do
  _ <- string "seeds: "
  seeds <- manyTill anyChar newline
  _ <- newline
  smaps <- manyTill seedMap eof
  return (SeedFile (splitToInt seeds) smaps)

seedMap :: GenParser Char st SeedMap
seedMap = do
  title <- seedMapTitle
  maps <- manyTill mapping newline
  return (SeedMap title maps)

seedMapTitle :: GenParser Char st String
seedMapTitle = do
  title <- manyTill anyChar (try (char ' '))
  _ <- string "map:"
  _ <- newline
  return title

mapping :: GenParser Char st Mapping
mapping = do
  dest <- number
  _ <- space
  src <- number
  _ <- space
  len <- number
  _ <- newline
  return (Mapping dest src len)

number :: GenParser Char st Int
number = do
  nr <- many1 digit
  return (read nr)

splitToInt :: String -> [Int]
splitToInt xs = read <$> split ' ' xs

split :: Char -> String -> [String]
split _ "" = []
split c (x:xs) | x == c = split c xs
split c xs = takeWhile (/= c) xs : split c (dropWhile (/= c) xs)

process :: SeedFile -> Int
process (SeedFile seeds maps) = foldr min (head locations) locations
  where
    locations = translateSeedNr maps <$> expandSeeds seeds

expandSeeds :: [Int] -> [Int]
expandSeeds [] = []
expandSeeds (x:len:xs) = [x..(x+len-1)] ++ expandSeeds xs
expandSeeds _ = error "Seeds must come in pairs"

translateSeedNr :: [SeedMap] -> Int -> Int
translateSeedNr [] nr = nr
translateSeedNr ((SeedMap _ m):ms) nr = translateSeedNr ms (numberFromMappings m nr)

numberFromMappings :: [Mapping] -> Int -> Int
numberFromMappings [] nr = nr
numberFromMappings ((Mapping dest src len):_) nr | inRange src len nr = nr - src + dest
numberFromMappings (_:ms) nr = numberFromMappings ms nr

inRange :: Int -> Int -> Int -> Bool
inRange src len nr = src <= nr && nr < src + len
