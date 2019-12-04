module LibSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "matchingSequences" $ do
        context "with 11" $
            it "should be [11]" $
                matchingSequences "11" `shouldBe` ["11"]
        
        context "with 111" $
            it "should be [111]" $
                matchingSequences "111" `shouldBe` ["111"]

        context "with 1121" $
            it "should be [11, 2, 1]" $
                matchingSequences "1121" `shouldBe` ["11", "2", "1"]

        context "with 11213334" $
            it "should be [11, 2, 1, 333, 4]" $
                matchingSequences "11213334" `shouldBe` ["11", "2", "1", "333", "4"]
    
    describe "sequenceLengths" $ do
        context "with 11" $
            it "should be [2]" $
                sequenceLengths "11" `shouldBe` [2]
        
        context "with 111" $
            it "should be [3]" $
                sequenceLengths "111" `shouldBe` [3]

        context "with 1121" $
            it "should be [2, 1, 1]" $
                sequenceLengths "1121" `shouldBe` [2, 1, 1]

main :: IO ()
main = hspec spec
