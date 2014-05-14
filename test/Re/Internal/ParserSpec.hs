module Re.Internal.ParserSpec where

import Test.Hspec

import Re.Internal.Parser

spec :: Spec
spec = do
    describe "" $ do
        it "" $ do
            0 `shouldBe` 0

main :: IO()
main = do
    hspec spec
