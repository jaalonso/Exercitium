module Suma_de_digitos_de_cadena_Spec (main, spec) where

import Suma_de_digitos_de_cadena
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (String -> Int) -> Spec
specG sumaDigitos =
  it "e1" $
    sumaDigitos "SE 2431 X"  `shouldBe`  10

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDigitos1
  describe "def. 2" $ specG sumaDigitos2
  describe "def. 3" $ specG sumaDigitos3
  describe "def. 4" $ specG sumaDigitos4
  describe "equivalencia" $ it "p1" $ property prop_sumaDigitos
