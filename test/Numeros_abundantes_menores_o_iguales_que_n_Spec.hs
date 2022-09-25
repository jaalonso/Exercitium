module Numeros_abundantes_menores_o_iguales_que_n_Spec (main, spec) where

import Numeros_abundantes_menores_o_iguales_que_n
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG numerosAbundantesMenores = do
  it "e1" $
    numerosAbundantesMenores 50  `shouldBe`  [12,18,20,24,30,36,40,42,48]
  it "e2" $
    numerosAbundantesMenores 48  `shouldBe`  [12,18,20,24,30,36,40,42,48]

spec :: Spec
spec = do
  describe "def. 1" $ specG numerosAbundantesMenores1
  describe "def. 2" $ specG numerosAbundantesMenores2
  describe "def. 3" $ specG numerosAbundantesMenores3
  describe "def. 4" $ specG numerosAbundantesMenores4
  describe "equivalencia" $ it "p1" $ property prop_numerosAbundantesMenores
