module Suma_de_los_primeros_numeros_naturales_Spec (main, spec) where

import Suma_de_los_primeros_numeros_naturales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG suma =
  it "e1" $
    suma 3 `shouldBe` 6

spec :: Spec
spec = do
  describe "def. 1" $ specG suma1
  describe "def. 2" $ specG suma2
  describe "def. 3" $ specG suma3
  describe "def. 4" $ specG suma4
  describe "def. 5" $ specG suma5
  describe "equivalencia" $ it "p1" $ property prop_suma
