module Numeros_primos_de_Hilbert_Spec (main, spec) where

import Numeros_primos_de_Hilbert
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG primosH = 
  it "e1" $
    take 15 primosH `shouldBe` [5,9,13,17,21,29,33,37,41,49,53,57,61,69,73]

spec :: Spec
spec = do
  describe "def. 1" $ specG primosH1
  describe "def. 2" $ specG primosH2
  describe "def. 3" $ specG primosH3
  describe "equivalencia" $ it "p1" $ property prop_primosH
