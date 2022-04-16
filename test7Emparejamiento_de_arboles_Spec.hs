module Emparejamiento_de_arboles_Spec (main, spec) where

import Emparejamiento_de_arboles
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ((Int -> Int -> Int) -> Arbol Int -> Arbol Int -> Arbol Int) -> Spec
specG emparejaArboles = do
  it "e1" $
    emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
    `shouldBe` N 2 [N 8 []]
  it "e2" $
    emparejaArboles (+) ej1 ej2
    `shouldBe` N 4 [N 11 [],N 7 []]
  it "e3" $
    emparejaArboles (+) ej1 ej1
    `shouldBe` N 2 [N 12 [],N 6 [N 10 []]]

spec :: Spec
spec = do
  describe "def. 1" $ specG emparejaArboles1
  describe "def. 2" $ specG emparejaArboles2
  describe "equivalencia" $ it "p1" $ property prop_emparejaArboles
