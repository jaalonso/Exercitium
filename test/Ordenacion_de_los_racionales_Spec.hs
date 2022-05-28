module Ordenacion_de_los_racionales_Spec (main, spec) where

import Ordenacion_de_los_racionales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [(Integer,Integer)]) -> Spec
specG fraccionesOrd = do
  it "e1" $
    fraccionesOrd 4 `shouldBe`
    [(1,4),(1,3),(1,2),(2,3),(3,4)]
  it "e2" $
    fraccionesOrd 5 `shouldBe`
    [(1,5),(1,4),(1,3),(2,5),(1,2),(3,5),(2,3),(3,4),(4,5)]

spec :: Spec
spec = do
  describe "def. 1" $ specG fraccionesOrd1
  describe "def. 2" $ specG fraccionesOrd2
  describe "def. 3" $ specG fraccionesOrd3
  describe "def. 4" $ specG fraccionesOrd4
  describe "equivalencia" $ it "p1" $ property prop_fraccionesOrd
