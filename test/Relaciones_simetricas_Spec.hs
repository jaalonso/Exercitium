module Relaciones_simetricas_Spec (main, spec) where

import Relaciones_simetricas
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG simetrica' = do
  it "e1" $
    simetrica' (R ([1,3],[(1,1),(1,3),(3,1)]))  `shouldBe`  True
  it "e2" $
    simetrica' (R ([1,3],[(1,1),(1,3),(3,2)]))  `shouldBe`  False
  it "e3" $
    simetrica' (R ([1,3],[]))                   `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG simetrica
  describe "def. 2" $ specG simetrica2
  describe "def. 3" $ specG simetrica3
  describe "equivalencia" $ it "p1" $ property prop_simetrica
