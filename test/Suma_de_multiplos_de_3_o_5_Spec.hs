module Suma_de_multiplos_de_3_o_5_Spec (main, spec) where

import Suma_de_multiplos_de_3_o_5
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG euler1 = do
  it "e1" $
    euler1 10  `shouldBe` 23
  it "e2" $
    euler1 100 `shouldBe` 2318

spec :: Spec
spec = do
  describe "def. 1" $ specG euler1a
  describe "def. 2" $ specG euler1b
  describe "def. 3" $ specG euler1c
  describe "def. 4" $ specG euler1d
  describe "def. 5" $ specG euler1e
  describe "def. 6" $ specG euler1f
  describe "def. 7" $ specG euler1g
  describe "equivalencia" $ it "p1" $ property prop_euler1
