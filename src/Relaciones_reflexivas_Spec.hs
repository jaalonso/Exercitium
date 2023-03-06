module Relaciones_reflexivas_Spec (main, spec) where

import Relaciones_reflexivas
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG reflexiva' = do
  it "e1" $
    reflexiva' (R ([1,3],[(1,1),(1,3),(3,3)]))    `shouldBe`  True
  it "e2" $
    reflexiva' (R ([1,2,3],[(1,1),(1,3),(3,3)]))  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG reflexiva
  describe "def. 2" $ specG reflexiva2
  describe "def. 3" $ specG reflexiva3
  describe "def. 4" $ specG reflexiva4
  describe "equivalencia" $ it "p1" $ property prop_reflexiva
