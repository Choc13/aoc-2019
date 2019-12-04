module LibSpec where

import SpecHelper

spec :: Spec
spec =
    describe "Fuel" $ do
        context "with 1" $
            it "should be 0" $
                fuel 1 `shouldBe` 0
        
        context "with 0" $
            it "should be 0" $
                fuel 0 `shouldBe` 0

main :: IO ()
main = hspec spec
