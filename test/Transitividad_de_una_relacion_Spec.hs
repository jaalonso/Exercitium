module Transitividad_de_una_relacion_Spec (main, spec) where

import Transitividad_de_una_relacion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([(Int,Int)] -> Bool) -> Spec
specG transitiva = do
  it "e1" $
    transitiva [(1,1),(1,3),(3,1),(3,3),(5,5)]  `shouldBe`  True
  it "e2" $
    transitiva [(1,1),(1,3),(3,1),(5,5)]        `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG transitiva1
  describe "def. 2" $ specG transitiva2
  describe "def. 3" $ specG transitiva3
  describe "def. 4" $ specG transitiva4
  describe "equivalencia" $ it "p1" $ property prop_transitiva
