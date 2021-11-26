module PokerHandSpec where
import Test.Hspec
import PokerHand

spec :: Spec
spec = do
    describe "comparer 2 cartes" $ do
        it "2 est plus petite que 3" $ do
            "2s" `estPlusPetite` "3s"  `shouldBe` True

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
