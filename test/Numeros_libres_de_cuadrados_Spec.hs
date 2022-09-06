module Numeros_libres_de_cuadrados_Spec (main, spec) where

import Numeros_libres_de_cuadrados
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Bool) -> Spec
specG libreDeCuadrados = do
  it "e1" $
    libreDeCuadrados 70  `shouldBe`  True
  it "e2" $
    libreDeCuadrados 40  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG libreDeCuadrados1
  describe "def. 2" $ specG libreDeCuadrados2
  describe "def. 3" $ specG libreDeCuadrados3
  describe "def. 4" $ specG libreDeCuadrados4
  describe "equivalencia" $ it "p1" $ property prop_libreDeCuadrados
