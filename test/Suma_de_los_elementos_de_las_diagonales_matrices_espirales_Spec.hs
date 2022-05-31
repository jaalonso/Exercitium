module Suma_de_los_elementos_de_las_diagonales_matrices_espirales_Spec (main, spec) where

import Suma_de_los_elementos_de_las_diagonales_matrices_espirales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaDiagonales = do
  it "e1" $
    sumaDiagonales 1         `shouldBe`  1   
  it "e2" $
    sumaDiagonales 2         `shouldBe`  10  
  it "e3" $
    sumaDiagonales 3         `shouldBe`  25  
  it "e4" $
    sumaDiagonales 4         `shouldBe`  56  
  it "e5" $
    sumaDiagonales 5         `shouldBe`  101
  it "e6" $ 
    and [sumaDiagonales n == sumaDiagonales2 n | n <- [2..10]] `shouldBe` True

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDiagonales1
  describe "def. 2" $ specG sumaDiagonales2
  describe "def. 3" $ specG sumaDiagonales3
  describe "equivalencia" $ it "p1" $ property prop_sumaDiagonales
