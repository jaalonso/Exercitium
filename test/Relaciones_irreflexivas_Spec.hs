module Relaciones_irreflexivas_Spec (main, spec) where

import Relaciones_irreflexivas
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG irreflexiva' = do
  it "e1" $
    irreflexiva' (R ([1,2,3],[(1,2),(2,1),(2,3)]))  `shouldBe`  True
  it "e2" $
    irreflexiva' (R ([1,2,3],[(1,2),(2,1),(3,3)]))  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG irreflexiva
  describe "def. 2" $ specG irreflexiva2
  describe "def. 3" $ specG irreflexiva3
  describe "equivalencia" $ it "p1" $ property prop_irreflexiva
