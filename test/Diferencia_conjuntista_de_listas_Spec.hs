module Diferencia_conjuntista_de_listas_Spec (main, spec) where

import Diferencia_conjuntista_de_listas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG diferencia = do
  it "e1" $
    diferencia [3,2,5,6] [5,7,3,4]  `shouldBe`  [2,6]
  it "e1" $
    diferencia [3,2,5] [5,7,3,2] `shouldBe` []

spec :: Spec
spec = do
  describe "def. 1" $ specG diferencia1
  describe "def. 2" $ specG diferencia2
  describe "def. 3" $ specG diferencia3
  describe "def. 4" $ specG diferencia4
  describe "equivalencia" $ it "p1" $ property prop_diferencia
