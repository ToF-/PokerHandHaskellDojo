module PokerHandSpec where
import Test.Hspec
import PokerHand

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
        it "retourne Couleur si la main contient une couleur" $ do
            categorieDeMain (words "3h 9h 5h Ah 7h") `shouldBe` Couleur
        it "retourne MainPleine si la main contient une main pleine" $ do
            categorieDeMain (words "3h 3s 5h 5c 5d") `shouldBe` MainPleine
        it "retourne Carre si la main contient un carre" $ do
            categorieDeMain (words "3h 3s 3d 3c 5d") `shouldBe` Carre


    describe "trouverLaMainLaPlusForte" $ do
        it "retourner QuinteFlushRoyale quand les paramètres sont QuinteFlush, QuinteFlushRoyale" $ do
            trouverLaMainLaPlusForte (words "As Ks Ts Jd Qd") (words "Kh Jh Ah Th Qh") `shouldBe` QuinteFlushRoyale
