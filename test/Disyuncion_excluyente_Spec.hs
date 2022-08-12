module Disyuncion_excluyente_Spec (main, spec) where

import Disyuncion_excluyente
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Bool -> Bool -> Bool) -> Spec
specG xor = do
  it "e1" $
    xor True  True  `shouldBe` False
  it "e2" $
    xor True  False `shouldBe` True
  it "e3" $
    xor False True  `shouldBe` True
  it "e4" $
    xor False False `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG xor1
  describe "def. 2" $ specG xor2
  describe "def. 3" $ specG xor3
  describe "def. 4" $ specG xor4
  describe "def. 5" $ specG xor5
  describe "equivalencia" $ it "p1" $ property prop_xor
