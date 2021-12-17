module PokerHand where
import Data.List (sort,sortBy,group)
import Data.Ord (comparing)

type Main = [String]
type Carte = String
type Rang = Integer
type Couleur = Char

data Categorie = CarteHaute | Paire | DoublePaire |  Brelan | Quinte | Couleur | MainPleine | Carre | QuinteFlush | QuinteRoyale | QuinteFlushRoyale
    deriving (Show, Ord, Eq)

double :: Integer -> Integer
double x = 5

estPlusPetite :: Ord a => a -> a -> Bool
estPlusPetite = (<)

couleur :: Carte -> Couleur
couleur carte = carte!!1

extraireLaListeDesCouleurs :: Main -> [Couleur]
extraireLaListeDesCouleurs main = map couleur main

aUneCouleur :: Main -> Bool
aUneCouleur main = all (==couleur (main!!0)) (extraireLaListeDesCouleurs main)

rangTries :: Main -> [Rang]
rangTries = sort . (map rang)

aUneQuinte :: Main -> Bool
aUneQuinte main | sort (map rang main) == [2, 3, 4, 5, 14] = True
aUneQuinte main = and (zipWith estSuccesseur (rangTries main) (tail (rangTries main)))
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

aUneQuinteFlush :: Main -> Bool
aUneQuinteFlush main = aUneCouleur main && aUneQuinte main

aUneRoyalFlush :: Main -> Bool
aUneRoyalFlush main = aUneQuinteFlush main && minimum (rangTries main) == 10

regroupeParRang :: Main -> [[Rang]]
regroupeParRang = reverse . sortBy (comparing length) . group . rangTries

trouverLaMainLaPlusForte :: Main -> Main -> Categorie
trouverLaMainLaPlusForte _ _ = QuinteFlushRoyale

categorieDeMain :: Main -> Categorie
categorieDeMain main = prendEnCompteLaCouleur (categorieDeBase (regroupeParRang main)) main

prendEnCompteLaCouleur :: Categorie -> Main -> Categorie
prendEnCompteLaCouleur CarteHaute main | aUneCouleur main = Couleur
prendEnCompteLaCouleur Quinte main     | aUneCouleur main = QuinteFlush
prendEnCompteLaCouleur QuinteRoyale main     | aUneCouleur main = QuinteFlushRoyale
prendEnCompteLaCouleur autre _ = autre

categorieDeBase :: [[Rang]] -> Categorie
categorieDeBase [[_,_],[_],[_],[_]] = Paire
categorieDeBase [[_,_],[_,_],[_]] = DoublePaire
categorieDeBase [[_,_,_],[_],[_]] = Brelan
categorieDeBase [[_,_,_,_],[_]] = Carre
categorieDeBase [[_,_,_],[_,_]] = MainPleine
categorieDeBase [[14],[5],[4],[3],[2]] = Quinte
categorieDeBase [[14],[13],[12],[11],[10]] = QuinteRoyale
categorieDeBase [[a],[b],[c],[d],[e]] | a-e == 4 = Quinte
categorieDeBase _ = CarteHaute


