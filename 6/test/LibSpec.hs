module LibSpec where

import SpecHelper

spec :: Spec
spec =
    describe "parseInstruction" $ do
        context "with 99" $
            it "should be 99, 0, 0, 0" $
                parseInstruction 99 `shouldBe` Instruction { opCode = 99, paramMode1 = 0, paramMode2 = 0, paramMode3 = 0 }

        context "with 2" $
            it "should be 2, 0, 0, 0" $
                parseInstruction 2 `shouldBe` Instruction { opCode = 2, paramMode1 = 0, paramMode2 = 0, paramMode3 = 0 }

        context "with 1002" $
            it "should be 2, 0, 1, 0" $
                parseInstruction 1002 `shouldBe` Instruction { opCode = 2, paramMode1 = 0, paramMode2 = 1, paramMode3 = 0 }
            
        context "with 11101" $
            it "should be 1, 1, 1, 1" $
                parseInstruction 11101 `shouldBe` Instruction { opCode = 1, paramMode1 = 1, paramMode2 = 1, paramMode3 = 1 }

main :: IO ()
main = hspec spec
