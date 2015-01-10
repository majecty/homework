{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Test.Hspec
import Data.Aeson
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

import HW06

main :: IO()
main = hspec $ do
  describe "Prelude.heade" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "ynToBool" $ do
    it "Y to True" $ do
      ynToBool (String "Y") `shouldBe` (Bool True)
    it "N to False" $ do
      ynToBool (String "N") `shouldBe` (Bool False)
    it "other values does not change" $ do
      ynToBool (String "XXN") `shouldBe` (String "XXN")
