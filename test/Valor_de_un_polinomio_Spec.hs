module Valor_de_un_polinomio_Spec (main, spec) where

import Valor_de_un_polinomio
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Polinomio Int -> Int -> Int) -> Spec
specG valor = do
  it "e1" $
    valor ej_pol1 0  `shouldBe`  6
  it "e2" $
    valor ej_pol1 1  `shouldBe`  10
  it "e3" $
    valor ej_pol1 2  `shouldBe`  102
  it "p1" $
    property prop_valor

spec :: Spec
spec = do
  describe "def. 1" $ specG valor1
  describe "def. 2" $ specG valor2
  describe "def. 3" $ specG valor3
  describe "def. 4" $ specG valor4
