module Factorizaciones_de_numeros_de_Hilbert_Spec (main, spec) where

import Factorizaciones_de_numeros_de_Hilbert
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [[Integer]]) -> Spec
specG factorizacionesH = do
  it "e1" $
    factorizacionesH  25  `shouldBe`  [[5,5]]
  it "e2" $
    factorizacionesH  45  `shouldBe`  [[5,9]]
  it "e3" $
    factorizacionesH 441  `shouldBe`  [[9,49],[21,21]]

spec :: Spec
spec = do
  describe "def. 1" $ specG factorizacionesH1
  describe "def. 2" $ specG factorizacionesH2
  describe "def. 3" $ specG factorizacionesH3
  describe "equivalencia" $ it "p1" $ property prop_factorizacionesH
