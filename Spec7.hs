{-# LANGUAGE DeriveGeneric, OverloadedStrings, StandaloneDeriving #-}

import Test.Hspec
-- import Test.QuickCheck

import HW07

main :: IO()
main = hspec $ do
  describe "Prelude.heade" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
