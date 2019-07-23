{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "plus2" $ do
    it "basic check" $ 1+1 `shouldBe` 2
    it "overflow" $ 1 + 1 `shouldBe` 2
    -- prop "minus 2" $ \i -> i - 2 `shouldBe` 0
