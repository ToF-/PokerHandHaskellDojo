module PokerHandSpec where
import Test.Hspec
import PokerHand

spec :: Spec
spec = do
    describe "comparer 2 cartes" $ do
        it "2 est plus petite que 3" $ do
            "2s" `estPlusPetite` "3s"  `shouldBe` True

    describe "aUneRoyalFlush" $ do
        it "retourne faux si la main a moins de 5 cartes" $ do
            aUneRoyalFlush ["1s", "2s", "3s", "4s"] `shouldBe` False
