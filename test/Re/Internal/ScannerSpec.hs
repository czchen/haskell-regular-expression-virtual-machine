module Re.Internal.ScannerSpec where

import Test.Hspec

import Re.Internal.Scanner

spec :: Spec
spec = do
    describe "tokenize" $ do
        it "Simple Char" $ do
            tokenize "abcde" `shouldBe` (Right [Simple 'a', Simple 'b', Simple 'c', Simple 'd', Simple 'e'])

        it "AnyChar" $ do
            tokenize "a." `shouldBe` (Right [Simple 'a', AnyChar])

        it "ZeroOrOne" $ do
            tokenize "a?" `shouldBe` (Right [Simple 'a', ZeroOrOne])

        it "ZeroOrMore" $ do
            tokenize "a*" `shouldBe` (Right [Simple 'a', ZeroOrMore])

        it "OneOrMore" $ do
            tokenize "a+" `shouldBe` (Right [Simple 'a', OneOrMore])

        it "Parenthesis" $ do
            tokenize "(a)" `shouldBe` (Right [OpenParenthesis, Simple 'a', CloseParenthesis])

        it "Alternative" $ do
            tokenize "a|b" `shouldBe` (Right [Simple 'a', Alternative, Simple 'b'])

        it "Escape" $ do
            tokenize "\\?\\*\\+\\(\\)\\|\\\\" `shouldBe` (Right [Simple '?', Simple '*', Simple '+', Simple '(', Simple ')', Simple '|', Simple '\\'])

        it "Escape error" $ do
            tokenize "\\" `shouldBe` Left "Escape error at pos 0"

main :: IO()
main = do
    hspec spec
