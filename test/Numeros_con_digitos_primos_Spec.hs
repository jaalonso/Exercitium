module Numeros_con_digitos_primos_Spec (main, spec) where

import Numeros_con_digitos_primos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG numerosConDigitosPrimos = do
  it "e1" $
    take 22 numerosConDigitosPrimos `shouldBe`
    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]

spec :: Spec
spec = do
  describe "def. 1" $ specG numerosConDigitosPrimos1
  describe "def. 2" $ specG numerosConDigitosPrimos2
  describe "def. 3" $ specG numerosConDigitosPrimos3
  describe "def. 4" $ specG numerosConDigitosPrimos4
  describe "equivalencia" $ it "p1" $ property prop_numerosConDigitosPrimos
