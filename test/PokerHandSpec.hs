module PokerHandSpec where
import Test.Hspec
import PokerHand

spec :: Spec
spec = do
    describe "comparer 2 cartes" $ do
        it "2 est plus petite que 3" $ do
            "2s" `estPlusPetite` "3s"  `shouldBe` True

    describe "a une flush" $ do
        it "si la main a toutes ses cartes de la même couleur" $ do
            aUneFlush ["As", "2s", "8s", "4s", "Ts"] `shouldBe` True
            aUneFlush ["As", "Qs", "3h", "4s", "5s"] `shouldBe` False

    describe "a une straight" $ do
        it "si la main a des cartes qui se suivent" $ do
            aUneQuinte ["2d", "5s", "4s", "3s", "6d"] `shouldBe` True
            aUneQuinte ["2d", "8s", "4s", "5s", "6d"] `shouldBe` False
            aUneQuinte ["3s", "4s", "5s", "6d", "7d"] `shouldBe` True
            aUneQuinte ["7s", "8s", "9s", "Td", "Jd"] `shouldBe` True
            aUneQuinte ["9s", "Ks", "Ts", "Jd", "Qd"] `shouldBe` True
            aUneQuinte ["3s", "2s", "4s", "Ad", "5d"] `shouldBe` True
            aUneQuinte ["As", "Ks", "Ts", "Jd", "Qd"] `shouldBe` True

    describe "aUneQuinteFlush" $ do
        it "si la main a des rangs successifs et une couleur unique" $ do
            aUneQuinteFlush (words "6h 8h Th 7h 9h") `shouldBe` True
            aUneQuinteFlush (words "6h 8h Ts 7h 9h") `shouldBe` False

    describe "aUneRoyalFlush" $ do
        it "si la main a une straight flush finisant par l'As" $ do
            aUneRoyalFlush (words "6h 8h Th 7h 9h") `shouldBe` False
            aUneRoyalFlush (words "Kh Jh Ah Th Qh") `shouldBe` True

    describe "aUnePaire" $ do
        it "si la main a une paire" $ do
            aUnePaire (words "6h 8h 8d As Tc") `shouldBe` True
            aUnePaire (words "6h 8h 5d As Tc") `shouldBe` False
            aUnePaire (words "6h 8h Ad As Tc") `shouldBe` True
            aUnePaire (words "6h 3h 8d As 3c") `shouldBe` True

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


    describe "trouverLaMainLaPlusForte" $ do
        it "retourner QuinteFlushRoyale quand les paramètres sont QuinteFlush, QuinteFlushRoyale" $ do
            trouverLaMainLaPlusForte (words "As Ks Ts Jd Qd") (words "Kh Jh Ah Th Qh") `shouldBe` QuinteFlushRoyale
