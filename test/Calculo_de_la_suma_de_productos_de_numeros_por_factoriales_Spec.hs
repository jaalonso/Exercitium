module Calculo_de_la_suma_de_productos_de_numeros_por_factoriales_Spec (main, spec) where

import Calculo_de_la_suma_de_productos_de_numeros_por_factoriales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG suma = do
  it "e1" $
    suma 1  `shouldBe`  1
  it "e2" $
    suma 2  `shouldBe`  5
  it "e3" $
    suma 3  `shouldBe`  23
  it "e4" $
    suma 4  `shouldBe`  119
  it "e5" $
    suma 5  `shouldBe`  719

spec :: Spec
spec = do
  describe "def. 1" $ specG suma1
  describe "def. 2" $ specG suma2
  describe "def. 3" $ specG suma3
  describe "equivalencia" $ it "p1" $ property prop_suma
