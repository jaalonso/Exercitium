module Conjunto_de_divisores_Spec (main, spec) where

import Conjunto_de_divisores
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG divisores = do
  it "e1" $
    divisores 30  `shouldBe`  [1,2,3,5,6,10,15,30]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisores1
  describe "def. 2" $ specG divisores2
  describe "def. 3" $ specG divisores3
  describe "def. 4" $ specG divisores4
  describe "def. 5" $ specG divisores5
  describe "equivalencia" $ it "p1" $ property prop_divisores
