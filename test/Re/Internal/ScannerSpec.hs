module Re.Internal.ScannerSpec where

import Test.Hspec

import Re.Internal.Scanner

spec :: Spec
spec = do
    describe "tokenize" $ do
        it "Simple Char" $ do
            tokenize "abcde" `shouldBe` (Right [Simple 'a', Simple 'b', Simple 'c', Simple 'd', Simple 'e'])
        it "ZeroOrOne" $ do
            tokenize "a?" `shouldBe` (Right [Simple 'a', ZeroOrOne])

main :: IO()
main = do
    hspec spec
