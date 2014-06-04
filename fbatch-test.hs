module Main where

import Test.Hspec

main :: IO()
main = hspec $ do
  describe "its cool bro" $ do
    it "no seriously its cool" $ do
      "its cool" `shouldBe` "its cool"