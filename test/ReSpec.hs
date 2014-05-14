module ReSpec where

import Test.Hspec

import Re

spec :: Spec
spec = do
    describe "test" $ do
        context "basic regular express" $ do
            it "nothing special" $ do
                let pattern = "12345"
                let string = "12345"

                (match pattern string) `shouldBe` Just True

main :: IO()
main = do
    hspec spec
