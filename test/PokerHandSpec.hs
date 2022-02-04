module PokerHandSpec where
import Test.Hspec
import PokerHand
import Data.List (sort)

spec :: Spec
spec = do
    describe "comparer 2 cartes" $ do
        it "2 est plus petite que 3" $ do
            "2s" `estPlusPetite` "3s"  `shouldBe` True

    describe "aUneQuinteFlush" $ do
        it "si la main a des rangs successifs et une couleur unique" $ do
            aUneQuinteFlush (words "6h 8h Th 7h 9h") `shouldBe` True
            aUneQuinteFlush (words "6h 8h Ts 7h 9h") `shouldBe` False

    describe "aUneRoyalFlush" $ do
        it "si la main a une straight flush finisant par l'As" $ do
            aUneRoyalFlush (words "6h 8h Th 7h 9h") `shouldBe` False
            aUneRoyalFlush (words "Kh Jh Ah Th Qh") `shouldBe` True

    describe "categorieDeMain" $ do
        it "retourne Paire si la main est une paire" $ do
            categorieDeMain (words "6h 8h 8d As Tc") `shouldBe` Paire
        it "retourne CarteHaute si la main est une carte haute" $ do
            categorieDeMain (words "6h 8h 9d As Tc") `shouldBe` CarteHaute
        it "retourne DoublePaire si la main contient deux paires" $ do
            categorieDeMain (words "6h 8h 8d As Ac") `shouldBe` DoublePaire
        it "retourne Brelan si la main contient un brelan" $ do
            categorieDeMain (words "6h 8h 8d 8s Tc") `shouldBe` Brelan
        it "retourne Quinte si la main contient un quinte" $ do
            categorieDeMain (words "3h 4h 5d 6s 7c") `shouldBe` Quinte
        it "retourne Quinte si la main contient la quinte commençant par Un" $ do
            categorieDeMain (words "3h 4h 5d 2s Ac") `shouldBe` Quinte
        it "retourne Couleur si la main contient une couleur" $ do
            categorieDeMain (words "3h 9h 5h Ah 7h") `shouldBe` Couleur
        it "retourne MainPleine si la main contient une main pleine" $ do
            categorieDeMain (words "3h 3s 5h 5c 5d") `shouldBe` MainPleine
        it "retourne Carre si la main contient un carre" $ do
            categorieDeMain (words "3h 3s 3d 3c 5d") `shouldBe` Carre
        it "retourne QuinteFlush si la main contient une Quinte Flush" $ do
            categorieDeMain (words "3d 4d 5d 6d 7d") `shouldBe` QuinteFlush
        it "retourne QuinteFlushRoyale si la main contient une Quinte Flush Royale" $ do
            categorieDeMain (words "Ad Kd Qd Jd Td") `shouldBe` QuinteFlushRoyale

    describe "trouverLaMainLaPlusForte" $ do
        it "retourner QuinteFlushRoyale quand les paramètres sont QuinteFlush, QuinteFlushRoyale" $ do
            trouverLaMainLaPlusForte (words "9s Ks Ts Js Qs") (words "Kh Jh Ah Th Qh") `shouldBe` QuinteFlushRoyale
        it "retourner QuinteFlush quand les paramètres sont QuinteFlush, Carre" $ do
            trouverLaMainLaPlusForte (words "9s Ks Ts Js Qs") (words "3h 3s 3d 3c 5d") `shouldBe` QuinteFlush

        it "retourne Carre quand les deux mains sont des Carre" $ do
            trouverLaMainLaPlusForte (words "9s 9s 9s 9s Qh") (words "3h 3s 3d 3c 5d") `shouldBe` Carre

    describe "compareMains" $ do
        it "compare une Quinte Flush et un Carre" $ do
            compareMain (words "9s Ks Ts Js Qs") (words "3h 3s 3d 3c 5d") `shouldBe` GT
        it "compare une Paire de 8 + J et une Paire de 8 + K" $ do
            compareMain (words "6h 8h 8d Js 2c") (words "6h 8h 8d Ks 2c") `shouldBe` LT
            compareMain (words "6h 8h 8d Ks 2c") (words "6h 8h 8d Js 2c") `shouldBe` GT
            compareMain (words "6h 8h 8d Ks 2c") (words "5h 8h 8d Ks 2c") `shouldBe` GT
            compareMain (words "6h 8h 8d Ks 3c") (words "6h 8h 8d Ks 2c") `shouldBe` GT
        it "compare deux mains qui sont identiques" $ do
            compareMain (words "9s 9s 9s 9s 5h") (words "9h 9s 9d 9c 5d") `shouldBe` EQ
        it "compare deux mains de Carte Haute" $ do
            compareMain (words "6s 8c 9c Ac Th") (words "6h 8h 9d As Tc") `shouldBe` EQ
        it "compare main pleine et couleur" $ do
            compareMain ["Kc","9s","Ks","Kd","9d"] ["4d","2d","Kd","9d","6d"] `shouldBe` GT

    describe "meilleureCombinaison" $ do
        it "retourne la meilleure main de 5 parmi 7" $ do
            meilleureCombinaison (words "Kh Jh Ah Th Qh 2c 2s") `shouldBe` (Just (words "Ah Kh Qh Jh Th"))
            meilleureCombinaison (words "Kh Jh 9h Th Qh 2c 2s") `shouldBe` (Just (words "Kh Qh Jh Th 9h"))
            meilleureCombinaison (words "Kh Jd 8d Th Qh 2c 2s") `shouldBe` (Just ["Kh","Qh","Jd","2c","2s"])
            meilleureCombinaison (words "5h 6d") `shouldBe` Nothing
            meilleureCombinaison ["Kc","9s","Ks","Kd","9d","3c","6d"] `shouldBe` (Just ["Kc","Ks","Kd","9s","9d"])
            meilleureCombinaison ["4d","2d","Ks","Kd","9d","3c","6d"] `shouldBe` (Just ["Kd","9d","6d","4d","2d"])

    describe "classement" $ do
        it "trie la liste par meilleure combinaison" $ do
            classement ([
                    words "Kc 9s Ks Kd 9d 3c 6d",
                    words "9c Ah Ks Kd 9d 3c 6d",
                    words "Ac Qc Ks Kd 9d 3c",
                    words "9h 5s",
                    words "4d 2d Ks Kd 9d 3c 6d",
                    words "7s Ts Ks Kd 9d"])
                `shouldBe` [
                    words "Ac Qc Ks Kd 9d 3c",
                    words "9h 5s",
                    words "7s Ts Ks Kd 9d",
                    words "9c Ah Ks Kd 9d 3c 6d",
                    words "4d 2d Ks Kd 9d 3c 6d",
                    words "Kc 9s Ks Kd 9d 3c 6d"]

    describe "libellé d'un groupe de 7 cartes" $ do
        it "ajoute le libellé d'une main" $ do
            libelle (words "Kc 9s Ks Kd 9d 3c 6d") `shouldBe` "Kc 9s Ks Kd 9d 3c 6d Full House"
            libelle (words "4d 2d Ks Kd 9d 3c 6d") `shouldBe` "4d 2d Ks Kd 9d 3c 6d Flush"
