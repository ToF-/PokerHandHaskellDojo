module PokerHand where
import Data.List (sort)

type Main = [String]
type Carte = String
type Rang = Char
type Couleur = Char

double :: Integer -> Integer
double x = 5

estPlusPetite :: Ord a => a -> a -> Bool
estPlusPetite = (<)

couleur :: Carte -> Couleur
couleur carte = carte!!1

extraireLaListeDesCouleurs :: Main -> [Couleur]
extraireLaListeDesCouleurs main = map couleur main

aUneFlush :: Main -> Bool
aUneFlush main = all (==couleur (main!!0)) (extraireLaListeDesCouleurs main)

aUneStraight :: Main -> Bool
aUneStraight main = and (zipWith estSuccesseur rangTries (tail rangTries))
    where
        rangTries = sort (map rang main)
        estSuccesseur a b = b == succ a

rang :: Carte -> Rang
rang = head

aUneStraightFlush :: Main -> Bool
aUneStraightFlush main =
    length main >= 5 &&
    all (==couleur (main!!0)) (extraireLaListeDesCouleurs main)

