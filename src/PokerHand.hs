module PokerHand where
import Data.List (sort,sortBy,group, subsequences, intersperse)
import Data.Ord (comparing)
import Data.Function (on)

type Main = [String]
type Carte = String
type Cartes = [String]
type Rang = Integer
type Couleur = Char

data Categorie = CarteHaute | Paire | DoublePaire |  Brelan | Quinte | Couleur | MainPleine | Carre | QuinteFlush | QuinteRoyale | QuinteFlushRoyale
    deriving (Show,Eq, Ord)

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
trouverLaMainLaPlusForte m n = max (categorieDeMain m) (categorieDeMain n)

compareMain :: Main -> Main -> Ordering
compareMain m n | (compare `on` categorieDeMain) m n == EQ = (compare `on` rangTries) m n
compareMain m n = (compare `on` categorieDeMain) m n

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

mainTriee :: Main -> Main
mainTriee main = sortBy (\m n -> flip compare (rang m) (rang n)) main

meilleureCombinaison :: Cartes -> Maybe Main
meilleureCombinaison cartes | length cartes == 7 = (Just . mainTriee . last . sortBy compareMain . filter (\s -> length s == 5) . subsequences) cartes
meilleureCombinaison _ = Nothing

classement :: [Cartes] -> [Cartes]
classement = sortBy (comparing (fmap categorieDeMain . meilleureCombinaison))

libelle :: Cartes -> String
libelle cs = concat (intersperse " " cs) ++ libelleCategorie ((fmap categorieDeMain . meilleureCombinaison) cs)
    where
    libelleCategorie (Just Paire) = " Pair"
    libelleCategorie (Just CarteHaute) = " Highcard"
    libelleCategorie (Just DoublePaire) = " Two Pairs"
    libelleCategorie (Just Brelan) = " Three Of A Kind"
    libelleCategorie (Just Quinte) = " Straight"
    libelleCategorie (Just Couleur) = " Flush"
    libelleCategorie (Just MainPleine) = " Full House"
    libelleCategorie (Just Carre) = " Four Of A Kind"
    libelleCategorie (Just QuinteFlush) = " Straight Flush"
    libelleCategorie (Just QuinteFlushRoyale) = " Royal Flush"
    libelleCategorie Nothing = ""
