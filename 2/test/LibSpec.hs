module LibSpec where

import SpecHelper

spec :: Spec
spec =
    describe "Run" $ do
        context "with [1,0,0,0,99]" $
            it "should be [2,0,0,0,99]" $
                run 0 0 [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        
        context "with [2,3,0,3,99]" $
            it "should be [2,3,0,6,99]" $
                run 3 0 [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

        context "with [1,9,10,3,2,3,11,0,99,30,40,50]" $
            it "should be [3500,9,10,70,2,3,11,0,99,30,40,50]" $
                run 9 10 [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]

main :: IO ()
main = hspec spec
