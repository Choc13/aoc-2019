module LibSpec where

import           SpecHelper

spec :: Spec
spec = describe "toPolar" $ do
    context "with (0, -1)"
        $          it "should be (1, 0)"
        $          toPolar (0, -1)
        `shouldBe` (1, 0)

    context "with (1, -1)"
        $          it "should be (sqrt 2, pi/4)"
        $          toPolar (1, -1)
        `shouldBe` (sqrt 2, pi/4)
    
    context "with (1, 0)"
        $          it "should be (1, pi/2)"
        $          toPolar (1, 0)
        `shouldBe` (1, pi / 2)

    context "with (1, 1)"
        $          it "should be (sqrt 2, 3pi/4)"
        $          toPolar (1, 1)
        `shouldBe` (sqrt 2, 3 * pi / 4)

    context "with (0, 1)"
        $          it "should be (1, pi)"
        $          toPolar (0, 1)
        `shouldBe` (1, pi)

    context "with (-1, 1)"
        $          it "should be (sqrt 2, 5pi/4)"
        $          toPolar (-1, 1)
        `shouldBe` (sqrt 2, 5 * pi / 4)

    context "with (-1, 0)"
        $          it "should be (1, 3pi/2)"
        $          toPolar (-1, 0)
        `shouldBe` (1, 3 * pi / 2)

    context "with (-1, -1)"
        $          it "should be (sqrt 2, 7pi/4)"
        $          toPolar (-1, -1)
        `shouldBe` (sqrt 2, 7 * pi / 4)


main :: IO ()
main = hspec spec
