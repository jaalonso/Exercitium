module Suma_de_los_cuadrados_de_los_primeros_numeros_naturales_Spec (main, spec) where

import Suma_de_los_cuadrados_de_los_primeros_numeros_naturales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaDeCuadrados = do
  it "e1" $
    sumaDeCuadrados 3    `shouldBe`  14
  it "e2" $
    sumaDeCuadrados 100  `shouldBe`  338350

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDeCuadrados1
  describe "def. 2" $ specG sumaDeCuadrados2
  describe "def. 3" $ specG sumaDeCuadrados3
  describe "def. 4" $ specG sumaDeCuadrados4
  describe "def. 5" $ specG sumaDeCuadrados5
  describe "equivalencia" $ it "p1" $ property prop_sumaDeCuadrados
