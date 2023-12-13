{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.List (elemIndex, group, sort, sortOn)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ sum $ applyBids $ sortOn pluckHand $ parseHands input
  where
    pluckHand (h, _) = h

applyBids :: [(Hand, Int)] -> [Int]
applyBids xs = uncurry (*) <$> rankedBids
  where
    rankedBids = zip [1..] (pluckBid <$> xs)
    pluckBid (_, b) = b

parseHands :: String -> [(Hand, Int)]
parseHands s = parseHandAndBid <$> lines s

parseHandAndBid :: String -> (Hand, Int)
parseHandAndBid (a:b:c:d:e:' ':xs) = (Hand (Card <$> [a,b,c,d,e]), read xs)
parseHandAndBid xs = error $ "Unexpected hand data: " ++ xs

-- parseHand :: String -> Hand
-- parseHand [a, b, c, d, e] = Hand (Card <$> [a,b,c,d,e])
-- parseHand xs = error $ "Unexpected hand data: " ++ xs

data Hand = Hand [Card]

instance Show Hand where
  show :: Hand -> String
  show (Hand h) = (\(Card c) -> c) <$> h

instance Eq Hand where
  (==) :: Hand -> Hand -> Bool
  (==) (Hand h) (Hand h') = h == h'

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare (Hand h) (Hand h') = case compare (handType $ upgradeJokers (Hand h)) (handType $ upgradeJokers (Hand h')) of
    EQ -> compare h h'
    cmp -> cmp

data Card = Card Char

instance Show Card where
  show :: Card -> String
  show (Card c) = show c

instance Eq Card where
  (==) :: Card -> Card -> Bool
  (==) (Card c) (Card c') = c == c'

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare (Card c) (Card c') = compare (orderIndex c) (orderIndex c')
    where
      orderIndex x = fromMaybe 0 (elemIndex x order)
      order = reverse ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']

handType :: Hand -> Int
handType h | fiveOfAKind h = 6
handType h | fourOfAKind h = 5
handType h | fullHouse h = 4
handType h | threeOfAKind h = 3
handType h | twoPair h = 2
handType h | onePair h = 1
handType _ = 0

fiveOfAKind :: Hand -> Bool
fiveOfAKind = hasCardGroupOfSize 5

fourOfAKind :: Hand -> Bool
fourOfAKind = hasCardGroupOfSize 4

fullHouse :: Hand -> Bool
fullHouse h = hasCardGroupOfSize 3 h && hasCardGroupOfSize 2 h

threeOfAKind :: Hand -> Bool
threeOfAKind = hasCardGroupOfSize 3

twoPair :: Hand -> Bool
twoPair (Hand xs) = length (filter (\g -> length g == 2) $ group $ sort xs) == 2

onePair :: Hand -> Bool
onePair = hasCardGroupOfSize 2

hasCardGroupOfSize :: Int -> Hand -> Bool
hasCardGroupOfSize size (Hand xs) = any (\g -> length g == size) $ group $ sort xs

upgradeJokers :: Hand -> Hand
upgradeJokers (Hand cards) = Hand $ upgradeJokers' (length $ filter isJoker cards) (filter isNotJoker cards)
  where
    upgradeJokers' :: Int -> [Card] -> [Card]
    -- 5 jokers: replace them all with same card
    upgradeJokers' 5 [] = replicate 5 (Card 'A')
    -- 4 jokers: make them the same as the non-joker card
    upgradeJokers' 4 [c] = replicate 5 c
    -- 3 jokers: make them the same as one of the other two cards
    --           when these two are different we end up with 4-of-a-kind
    --           when these two are the same we end up with 5-of-a-kind
    upgradeJokers' 3 [a,b] = replicate 4 a ++ [b]
    -- 2 jokers:
    upgradeJokers' 2 [a,b,c]
      | a == b = replicate 4 a ++ [c]
      | a == c = replicate 4 a ++ [b]
      | b == c = replicate 4 b ++ [a]
      | otherwise = replicate 3 highest ++ rest
      where
        rest = filter (/= highest) [a,b,c]
        highest = foldl max a [a,b,c]
    -- 1 joker: check which of the other cards has the largest count,
    --          replace joker with that card.
    --          - when all cards are different => one-pair (but make sure to pick the highest)
    --          - when there's an existing pair => three-of-a-kind
    --          - when two existing pairs => full-house
    --          - when three of the same => four-of-a-kind
    --          - when four of the same => five-of-a-kind
    upgradeJokers' 1 xs
      | fourOfAKind (Hand xs) = repeatLargestGroup
      | threeOfAKind (Hand xs) = repeatLargestGroup
      | twoPair (Hand xs) = highestOfPair : xs
      | onePair (Hand xs) = repeatLargestGroup
      | otherwise = highest : xs
      where
        repeatLargestGroup = head (last $ sortOn length $ group $ sort xs) : xs
        highest = maximum xs
        highestOfPair = maximum (head <$> group (sort xs))
    -- no jokers: do nothing
    upgradeJokers' 0 xs = xs
    upgradeJokers' _ _ = error "Incorrect number of cards"

isJoker :: Card -> Bool
isJoker (Card c) = c == 'J'

isNotJoker :: Card -> Bool
isNotJoker c = not (isJoker c)
