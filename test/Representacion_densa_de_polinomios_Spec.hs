module Representacion_densa_de_polinomios_Spec (main, spec) where

import Representacion_densa_de_polinomios
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [(Int,Int)]) -> Spec
specG densa = do
  it "e1" $
    densa [6,0,-5,4,-7]  `shouldBe`  [(4,6),(2,-5),(1,4),(0,-7)]
  it "e2" $
    densa [6,0,0,3,0,4]  `shouldBe`  [(5,6),(2,3),(0,4)]
  it "e3" $
    densa [0]            `shouldBe`  [(0,0)]

spec :: Spec
spec = do
  describe "def. 1" $ specG densa1
  describe "def. 2" $ specG densa2
  describe "def. 3" $ specG densa3
  describe "def. 4" $ specG densa4
  describe "equivalencia" $ it "p1" $ property prop_densa
