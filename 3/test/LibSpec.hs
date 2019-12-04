module LibSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "distanceBetween" $ do
        context "with (0, 0) (1, 0)" $
            it "should be 1" $
                distanceBetween (Point 0 0) Point { x = 0, y = 1 } `shouldBe` 1
        
        context "with (0, 1) (0, 0)" $
            it "should be 1" $
                distanceBetween Point  { x = 0, y = 1 } Point { x = 0, y = 0 } `shouldBe` 1
    
    describe "generateSegment" $ do
        context "with (0, 0) U1" $
            it "should be [(0, 1)]" $
                generateSegment (Point 0 0) "U1" `shouldBe` [Point{x = 0, y = 1}]
        
        context "with (0, 0) U2" $
            it "should be [(0, 2), (0, 1)]" $
                generateSegment (Point 0 0) "U2" `shouldBe` [Point {x = 0, y = 2}, Point{x = 0, y = 1}]

    describe "generateLine" $ do
        context "with [R1,U2]" $
            it "should be [(0, 0), (1, 0), (1, 1), (1, 2)]" $
                generateLine ["R1","U2"] `shouldBe` reverse [Point{x = 0, y = 0}, Point{x = 1, y = 0}, Point{x = 1, y = 1}, Point{x = 1, y = 2}]
        
        context "with [D1,L1]" $
            it "should be [(0, 0), (0, -1), (-1, -1)]" $
                generateLine ["D1","L1"] `shouldBe` reverse [Point{x = 0, y = 0}, Point{x = 0, y = -1}, Point{x = -1, y = -1}]

main :: IO ()
main = hspec spec
