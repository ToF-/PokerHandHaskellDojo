module PokerHand where

double :: Integer -> Integer
double x = 5

estPlusPetite :: Ord a => a -> a -> Bool
estPlusPetite = (<)

extraireLaCouleurDUneCarte :: String -> Char
extraireLaCouleurDUneCarte carte = carte!!1

extraireLaListeDesCouleurs :: [String] -> [Char]
extraireLaListeDesCouleurs main = map extraireLaCouleurDUneCarte main

aUneFlush :: [String] -> Bool
aUneFlush main = all (==extraireLaCouleurDUneCarte (main!!0)) (extraireLaListeDesCouleurs main)

aUneStraight :: [String] -> Bool
aUneStraight ["2d", "3s", "4s", "5s", "6d"] = True
aUneStraight ["3s", "4s", "5s", "6d", "7d"] = True
aUneStraight _ = False

suite :: [String] -> String
suite = map (!!0)

aUneStraightFlush :: [String] -> Bool
aUneStraightFlush main =
    length main >= 5 &&
    all (==extraireLaCouleurDUneCarte (main!!0)) (extraireLaListeDesCouleurs main)

