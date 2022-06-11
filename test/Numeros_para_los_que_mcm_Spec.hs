module Numeros_para_los_que_mcm_Spec (main, spec) where

import Numeros_para_los_que_mcm
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG especiales = do
  it "e1" $
    take 10 especiales `shouldBe` [1,6,10,12,14,15,18,20,21,22]

spec :: Spec
spec = do
  describe "def. 1" $ specG especiales1
  describe "def. 2" $ specG especiales2
  describe "def. 3" $ specG especiales3
  describe "equivalencia" $ it "p1" $ property prop_especiales
