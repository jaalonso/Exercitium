module Suma_de_los_digitos_de_un_numero_Spec (main, spec) where

import Suma_de_los_digitos_de_un_numero
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaDigitos = do
  it "e1" $
    sumaDigitos 3     `shouldBe`  3
  it "e2" $
    sumaDigitos 2454  `shouldBe` 15
  it "e3" $
    sumaDigitos 20045 `shouldBe` 11

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDigitos1
  describe "def. 2" $ specG sumaDigitos2
  describe "def. 3" $ specG sumaDigitos3
  describe "def. 4" $ specG sumaDigitos4
  describe "equivalencia" $ it "p1" $ property prop_sumaDigitos
