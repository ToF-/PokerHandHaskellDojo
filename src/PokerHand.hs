module PokerHand where

double :: Integer -> Integer
double x = 5

estPlusPetite :: Ord a => a -> a -> Bool
estPlusPetite = (<)

extraireLaCouleurDUneCarte :: String -> Char
extraireLaCouleurDUneCarte carte = carte!!1

extraireLaListeDesCouleurs :: [String] -> [Char]
extraireLaListeDesCouleurs main = map extraireLaCouleurDUneCarte main

aUneRoyalFlush :: [String] -> Bool
aUneRoyalFlush main = (extraireLaListeDesCouleurs main) == "sssss"
