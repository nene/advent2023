module Main (main) where

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ show (process $ safeParse input)

process :: [Card] -> Int
process cards = sum $ score cards <$> cards

score :: [Card] -> Card -> Int
score cards (Card i w h) = 1 + sum (score cards <$> extraCards)
  where
    extraCards = take (winCount w h) (drop i cards)

winCount :: [Int] -> [Int] -> Int
winCount winners havers = length $ filter (`elem` winners) havers

safeParse :: String -> [Card]
safeParse xs = case parse cardList "(unknown)" xs of
  Left _ -> []
  Right result -> result

data Card = Card Int [Int] [Int]

instance Show Card where
  show (Card i a b) = "Card " ++ show i ++ ": " ++ show a ++ " | " ++ show b

cardList :: GenParser Char st [Card]
cardList = manyTill card eof

card :: GenParser Char st Card
card = do
  _ <- string "Card"
  _ <- ws
  i <- number
  _ <- string ":"
  _ <- ws
  winners <- manyTill anyChar (try (string " | "))
  havers <- manyTill anyChar (try newline)
  return (Card (read i) (toNumbers winners) (toNumbers havers))

toNumbers :: String -> [Int]
toNumbers xs = read <$> split ' ' xs

number :: GenParser Char st String
number = many1 (oneOf "0123456789")

ws :: GenParser Char st ()
ws = do
  space >> spaces

split :: Char -> String -> [String]
split _ "" = []
split c (x:xs) | x == c = split c xs
split c xs = takeWhile (/= c) xs : split c (dropWhile (/= c) xs)
