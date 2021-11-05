module PokerHandSpec where
import Test.Hspec

spec :: Spec
spec = do
    describe "test bidon" $ do
        it "pass" $ do
            2+2 `shouldBe` 4