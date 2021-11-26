module PokerHand where
import Data.List (sort)

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
aUneStraight s | suite s == "23456" = True
aUneStraight s | suite s == "34567" = True
aUneStraight _ = False

suite :: [String] -> String
suite = sort . map (!!0)

aUneStraightFlush :: [String] -> Bool
aUneStraightFlush main =
    length main >= 5 &&
    all (==extraireLaCouleurDUneCarte (main!!0)) (extraireLaListeDesCouleurs main)

