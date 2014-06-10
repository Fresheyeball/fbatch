module FbatchSpec where

import Test.Hspec
--import Test.QuickCheck
import Fbatch

fbatchSpec :: Spec
fbatchSpec = do

  describe "replaceItoken should work like replace" $ do
    it "if no token is found" $ do
      (replaceItoken "a" "b" ("ccbbaa", 0)) `shouldBe` "ccbbbb"
      (replaceItoken "c" "x" ("ccbbaa", 0)) `shouldBe` "xxbbaa"
      (replaceItoken "b" "!" ("ccbbaa", 0)) `shouldBe` "cc!!aa"
    it "if token is found its replaced by the number" $ do
      (replaceItoken "z" "-#{i}" ("ccbzaa", 6)) `shouldBe` "ccb-6aa"
      (replaceItoken "a" "x#{i}" ("ccbbaa", 0)) `shouldBe` "ccbbx0x0"

  describe "getDeltas" $ do
    it "should use replace and return tuples" $ do
      let r    = "a"
      let r'   = "b"
      let str  = "accdaaee"
      let str' = replaceItoken r r' (str, 0)
      (getDeltas r r' [str]) `shouldBe` [(str,str')]
