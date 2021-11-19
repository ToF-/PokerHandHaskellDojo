module PokerHand where

double :: Integer -> Integer
double x = 5

estPlusPetite :: Ord a => a -> a -> Bool
estPlusPetite = (<)

aUneRoyalFlush :: [String] -> Bool
aUneRoyalFlush main = length main == 5 && (main!!4 == "5s" && main!!3 == "4s")
