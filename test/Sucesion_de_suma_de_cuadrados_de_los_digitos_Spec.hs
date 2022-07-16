module Sucesion_de_suma_de_cuadrados_de_los_digitos_Spec (main, spec) where

import Sucesion_de_suma_de_cuadrados_de_los_digitos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG sucSumaCuadradosDigitos = do
  it "e1" $
    take 20 (sucSumaCuadradosDigitos1 2000)
    `shouldBe` [2000,4,16,37,58,89,145,42,20,4,16,37,58,89,145,42,20,4,16,37]
  it "e2" $
    take 20 (sucSumaCuadradosDigitos 1976)
    `shouldBe` [1976,167,86,100,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

spec :: Spec
spec = do
  describe "def. 1" $ specG sucSumaCuadradosDigitos1
  describe "def. 2" $ specG sucSumaCuadradosDigitos2
  describe "def. 3" $ specG sucSumaCuadradosDigitos3
  describe "def. 4" $ specG sucSumaCuadradosDigitos4
  describe "equivalencia" $ it "p1" $ property prop_sucSumaCuadradosDigitos
