module ReSpec where

import Test.Hspec

import Re

spec :: Spec
spec = do
    describe "Re" $ do
        context "match" $ do
            it "broken pattern" $ do
                (match "++" "12345") `shouldBe` Nothing
                (match "1++" "12345") `shouldBe` Nothing

            it "no special charater pattern" $ do
                (match "12345" "12345") `shouldBe` Just True
                (match "12345" "1234") `shouldBe` Just False

            it "*" $ do
                (match "a*b" "b") `shouldBe` Just True
                (match "a*b" "ab") `shouldBe` Just True
                (match "a*b" "aab") `shouldBe` Just True

            it "?" $ do
                (match "a?b" "b") `shouldBe` Just True
                (match "a?b" "ab") `shouldBe` Just True
                (match "a?b" "aab") `shouldBe` Just False

            it "+" $ do
                (match "a+b" "b") `shouldBe` Just False
                (match "a+b" "ab") `shouldBe` Just True
                (match "a+b" "aab") `shouldBe` Just True

main :: IO()
main = do
    hspec spec
