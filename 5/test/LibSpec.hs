module LibSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "foo" $ do
        context "with bar" $
            it "should be baz" $
                far "bar" `shouldBe` "baz"

main :: IO ()
main = hspec spec
