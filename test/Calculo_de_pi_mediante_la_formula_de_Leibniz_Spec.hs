module Calculo_de_pi_mediante_la_formula_de_Leibniz_Spec (main, spec) where

import Calculo_de_pi_mediante_la_formula_de_Leibniz
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: (Int -> Double) -> Spec
specG1 calculaPi = do
  it "e1" $
    calculaPi 3    `shouldBe`  2.8952380952380956
  it "e2" $
    calculaPi 300  `shouldBe`  3.1449149035588526

specG2 :: (Double -> Int) -> Spec
specG2 errorPi = do
  it "e1" $
    errorPi 0.1    `shouldBe`    9
  it "e2" $
    errorPi 0.01   `shouldBe`   99
  it "e3" $
    errorPi 0.001  `shouldBe`  999

spec :: Spec
spec = do
  describe "def. 1" $ specG1 calculaPi1
  describe "def. 2" $ specG1 calculaPi2
  describe "def. 3" $ specG1 calculaPi3
  describe "equivalencia" $ it "p1" $ property prop_calculaPi
  describe "def. 1" $ specG2 errorPi1
  describe "def. 2" $ specG2 errorPi2
  describe "equivalencia" $ it "p1" $ property prop_errorPi
