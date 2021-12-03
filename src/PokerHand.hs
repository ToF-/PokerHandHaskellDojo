module PokerHand where
import Data.List (sort)

type Main = [String]
type Carte = String
type Rang = Integer
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

rangTries :: Main -> [Rang]
rangTries = sort . (map rang)

aUneStraight :: Main -> Bool
aUneStraight main | sort (map rang main) == [2, 3, 4, 5, 14] = True
aUneStraight main = and (zipWith estSuccesseur (rangTries main) (tail (rangTries main)))
    where
        estSuccesseur a b = b == succ a

rang :: Carte -> Rang
rang = valeur . head
    where
        valeur '2' = 2
        valeur '3' = 3
        valeur '4' = 4
        valeur '5' = 5
        valeur '6' = 6
        valeur '7' = 7
        valeur '8' = 8
        valeur '9' = 9
        valeur 'T' = 10
        valeur 'J' = 11
        valeur 'Q' = 12
        valeur 'K' = 13
        valeur 'A' = 14

aUneStraightFlush :: Main -> Bool
aUneStraightFlush main = aUneFlush main && aUneStraight main

aUneRoyalFlush :: Main -> Bool
aUneRoyalFlush main = aUneStraightFlush main && minimum (rangTries main) == 10

aUnePaire :: Main -> Bool
aUnePaire main = rangTries main == [6,8,8,10,14]
aUnePaire main = rangTries main == [6,8,10,14,14]