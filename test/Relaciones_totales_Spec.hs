module Relaciones_totales_Spec (main, spec) where

import Relaciones_totales
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck (property)

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG total' = do
  it "e1" $
    total' (R ([1,3],[(1,1),(3,1),(3,3)]))  `shouldBe`  True
  it "e2" $
    total' (R ([1,3],[(1,1),(3,1)]))        `shouldBe`  False
  it "e3" $
    total' (R ([1,3],[(1,1),(3,3)]))        `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG total
  describe "def. 2" $ specG total2
  describe "def. 3" $ specG total3
  describe "equivalencia" $ it "p1" $ property prop_total
