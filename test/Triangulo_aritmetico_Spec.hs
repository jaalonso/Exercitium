module Triangulo_aritmetico_Spec (main, spec) where

import Triangulo_aritmetico
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [[Integer]]) -> Spec
specG triangulo = do
  it "e1" $
    triangulo 3  `shouldBe`  [[1],[2,3],[4,5,6]]
  it "e2" $
    triangulo 4  `shouldBe`  [[1],[2,3],[4,5,6],[7,8,9,10]]

spec :: Spec
spec = do
  describe "def. 1" $ specG triangulo1
  describe "def. 2" $ specG triangulo2
  describe "def. 3" $ specG triangulo3
  describe "equivalencia" $ it "p1" $ property prop_triangulo
