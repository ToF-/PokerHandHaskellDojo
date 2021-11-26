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
            aUneFlush ["1s", "2s", "3s", "4s", "5s"] `shouldBe` True
            aUneFlush ["1s", "2s", "3h", "4s", "5s"] `shouldBe` False

    describe "a une straight" $ do
        it "si la main a des cartes qui se suivent" $ do
            aUneStraight ["2d", "5s", "4s", "3s", "6d"] `shouldBe` True
            aUneStraight ["2d", "8s", "4s", "5s", "6d"] `shouldBe` False
            aUneStraight ["3s", "4s", "5s", "6d", "7d"] `shouldBe` True
            aUneStraight ["7s", "8s", "9s", "Td", "Jd"] `shouldBe` True
            aUneStraight ["9s", "Ks", "Ts", "Jd", "Qd"] `shouldBe` True



    describe "aUneStraightFlush" $ do
        it "retourne faux si la main a moins de 5 cartes" $ do
            aUneStraightFlush ["1s", "2s", "3s", "4s"] `shouldBe` False

        it "retourne vrai si la main correspond à 1s,2s,3s,4s,5s" $ do
            aUneStraightFlush ["1s", "2s", "3s", "4s", "5s"] `shouldBe` True

        it "retourne vrai si la main correspond à 6s,7s,8s,9s,Ts" $ do
            aUneStraightFlush ["6s", "7s", "8s", "9s", "Ts"] `shouldBe` True

        it "retourne vrai si la main correspond à 6h,7h,8h,9h,Th" $ do
            aUneStraightFlush ["6h", "7h", "8h", "9h", "Th"] `shouldBe` True

        it "retourne vrai si la main a plus de 5 cartes de la même couleur" $ do
            aUneStraightFlush ["6h", "7h", "8h", "9h", "Th", "Jh", "Qh"] `shouldBe` True
