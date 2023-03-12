module Relaciones_transitivas_Spec (main, spec) where

import Relaciones_transitivas
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG transitiva' = do
  it "e1" $
    transitiva' (R ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])) `shouldBe` True
  it "e2" $
    transitiva' (R ([1,3,5],[(1,1),(1,3),(3,1),(5,5)]))       `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG transitiva1
  describe "def. 2" $ specG transitiva2
  describe "equivalencia" $ it "p1" $ property prop_transitiva
