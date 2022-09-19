module Divisores_de_un_numero_Spec (main, spec) where

import Divisores_de_un_numero
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG divisores =
  it "e1" $
    divisores 30  `shouldBe`  [1,2,3,5,6,10,15,30]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisores1
  describe "def. 2" $ specG divisores2
  describe "def. 3" $ specG divisores3
  describe "def. 4" $ specG divisores4
  describe "def. 5" $ specG divisores5
  describe "def. 6" $ specG divisores6
  describe "def. 7" $ specG divisores7
  describe "def. 8" $ specG divisores8
  describe "def. 9" $ specG divisores9
  describe "def. 10" $ specG divisores10
  describe "def. 11" $ specG divisores11
  describe "equivalencia" $ it "p1" $ property prop_divisores
