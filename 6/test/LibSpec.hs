module LibSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "answer" $
        context "with testcase" $
            it "should be answer" $
                answer ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"] `shouldBe` 42

    describe "answer2" $ do
        context "with test case" $
            it "should be answer" $
                answer2 ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"] `shouldBe` ("D", 6)

        context "pathTo" $
            it "should be answer" $
                pathTo (buildMap ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]) "YOU" "COM" `shouldBe` [("COM",7),("B",6),("C",5),("D",4),("E",3),("J",2),("K",1),("YOU",0)]

main :: IO ()
main = hspec spec
