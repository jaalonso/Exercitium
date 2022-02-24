module Reconocimiento_de_potencias_de_4_Spec (main, spec) where

import Reconocimiento_de_potencias_de_4
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Bool) -> Spec
specG esPotenciaDe4 = do
  it "e1" $
    esPotenciaDe4 1024 `shouldBe` True
  it "e2" $
    esPotenciaDe4  102 `shouldBe` False
  it "e3" $
    esPotenciaDe4   64 `shouldBe` True
  it "e4" $ do
      property $ forAll (arbitrary `suchThat` (>=0)) $ \x ->
        esPotenciaDe4 (4^(x :: Integer)) `shouldBe` True
  it "e5" $ do
      property $ forAll (arbitrary `suchThat` (>=0)) $ \x ->
        esPotenciaDe4 (4^(x :: Integer) + 1) `shouldBe` False


spec :: Spec
spec = do
  describe "def. 1" $ specG esPotenciaDe4_1
  describe "def. 2" $ specG esPotenciaDe4_2
  describe "def. 3" $ specG esPotenciaDe4_3
  describe "def. 4" $ specG esPotenciaDe4_4
