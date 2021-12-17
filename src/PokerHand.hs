module PokerHand where
import Data.List (sort,sortBy,group)
import Data.Ord (comparing)

type Main = [String]
type Carte = String
type Rang = Integer
type Couleur = Char

data Categorie = CarteHaute | Paire | DoublePaire |  Brelan | Quinte | Couleur | MainPleine | Carre | QuinteFlush | QuinteFlushRoyale
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

aUneDoublePaire :: Main -> Bool
aUneDoublePaire main = aUneDoublePaireDeRangs (regroupeParRang main)
    where aUneDoublePaireDeRangs [[_,_],[_,_],[_]] = True
          aUneDoublePaireDeRangs _ = False

aUnePaire :: Main -> Bool
aUnePaire main = aUnePaireDeRangs (regroupeParRang main)
    where
        aUnePaireDeRangs [[_,_],[_],[_],[_]] = True
        aUnePaireDeRangs _ = False

aUnBrelan :: Main -> Bool
aUnBrelan main = aUnBrelanDeRangs (regroupeParRang main)
    where
        aUnBrelanDeRangs [[_,_,_],[_],[_]] = True
        aUnBrelanDeRangs _ = False

aUneMainPleine :: Main -> Bool
aUneMainPleine main = aUneMainPleineDeRangs (regroupeParRang main)
    where
        aUneMainPleineDeRangs [[_,_,_],[_,_]] = True
        aUneMainPleineDeRangs _ = False

aUnCarre :: Main -> Bool
aUnCarre main = aUnCarreDeRangs (regroupeParRang main)
    where
        aUnCarreDeRangs [[_,_,_,_],[_]] = True
        aUnCarreDeRangs _ = False

regroupeParRang :: Main -> [[Rang]]
regroupeParRang = reverse . sortBy (comparing length) . group . rangTries

trouverLaMainLaPlusForte :: Main -> Main -> Categorie
trouverLaMainLaPlusForte _ _ = QuinteFlushRoyale

categorieDeMain :: Main -> Categorie
categorieDeMain m | aUnePaire m = Paire
                  | aUneDoublePaire m = DoublePaire
                  | aUnBrelan m = Brelan
                  | aUneQuinte m = Quinte
                  | aUneMainPleine m = MainPleine
                  | aUneCouleur m = Couleur
                  | aUnCarre m = Carre
                  | otherwise = CarteHaute
