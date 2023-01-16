module Transformaciones_pilas_listas_Spec (main, spec) where

import Transformaciones_pilas_listas
import TAD.PilaConListas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: ([Int] -> Pila Int) -> Spec
specG1 listaApila' =
  it "e1" $
    show (listaApila' [3, 2, 5]) `shouldBe` "5 | 2 | 3"

specG2 :: (Pila Int -> [Int]) -> Spec
specG2 pilaAlista' =
  it "e1" $
    pilaAlista' (apila 5 (apila 2 (apila 3 vacia)))
    `shouldBe` [3, 2, 5]

spec :: Spec
spec = do
  describe "def. listaApila 1" $ specG1 listaApila
  describe "def. listaApila 2" $ specG1 listaApila2
  describe "def. listaApila 3" $ specG1 listaApila3
  describe "def. listaApila 4" $ specG1 listaApila4
  describe "def. listaApila 5" $ specG1 listaApila5
  describe "equivalencia" $ it "p1" $ property prop_listaApila
  describe "def. pilaAlista 1" $ specG2 pilaAlista
  describe "def. pilaAlista 2" $ specG2 pilaAlista2
  describe "equivalencia" $ it "p1" $ property prop_pilaAlista
