module PokerHandSpec where
import Test.Hspec
import PokerHand

spec :: Spec
spec = do
    describe "test bidon" $ do
        it "pass" $ do
            double 2 `shouldBe` 5


