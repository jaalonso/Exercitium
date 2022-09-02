module Interseccion_conjuntista_de_listas_Spec (main, spec) where

import Interseccion_conjuntista_de_listas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG interseccion = do
  it "e1" $
    interseccion [3,2,5] [5,7,3,4]  `shouldBe`  [3,5]
  it "e2" $
    interseccion [3,2,5] [9,7,6,4]  `shouldBe`  []

spec :: Spec
spec = do
  describe "def. 1" $ specG interseccion1
  describe "def. 2" $ specG interseccion2
  describe "def. 3" $ specG interseccion3
  describe "def. 4" $ specG interseccion4
  describe "equivalencia" $ it "p1" $ property prop_interseccion
