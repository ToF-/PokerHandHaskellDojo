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
            aUneStraight ["2d", "5s", "4s", "3s", "6d"] `shouldBe` True
            aUneStraight ["2d", "8s", "4s", "5s", "6d"] `shouldBe` False
            aUneStraight ["3s", "4s", "5s", "6d", "7d"] `shouldBe` True
            aUneStraight ["7s", "8s", "9s", "Td", "Jd"] `shouldBe` True
            aUneStraight ["9s", "Ks", "Ts", "Jd", "Qd"] `shouldBe` True
            aUneStraight ["3s", "2s", "4s", "Ad", "5d"] `shouldBe` True
            aUneStraight ["As", "Ks", "Ts", "Jd", "Qd"] `shouldBe` True

    describe "aUneStraightFlush" $ do
        it "si la main a des rangs successifs et une couleur unique" $ do
            aUneStraightFlush (words "6h 8h Th 7h 9h") `shouldBe` True
            aUneStraightFlush (words "6h 8h Ts 7h 9h") `shouldBe` False

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
