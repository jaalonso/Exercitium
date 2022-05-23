module Densidad_de_numeros_abundantes_Spec (main, spec) where

import Densidad_de_numeros_abundantes
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> (Double,Double,Double)) -> Spec
specG densidades = do
  it "e1" $
    densidades 100     `shouldBe`  (0.22,    2.0e-2, 0.76)
  it "e2" $
    densidades 1000    `shouldBe`  (0.246,   3.0e-3, 0.751)

spec :: Spec
spec = do
  describe "def. 1" $ specG densidades1
  describe "def. 2" $ specG densidades2
  describe "def. 3" $ specG densidades3
  describe "def. 4" $ specG densidades4
  describe "def. 5" $ specG densidades5
  describe "equivalencia" $ it "p1" $ property prop_densidades
