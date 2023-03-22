module Relaciones_antisimetricas_Spec (main, spec) where

import Relaciones_antisimetricas
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Bool) -> Spec
specG antisimetrica' = do
  it "e1" $
    antisimetrica' (R ([1,2],[(1,2)]))        `shouldBe`  True
  it "e2" $
    antisimetrica' (R ([1,2],[(1,2),(2,1)]))  `shouldBe`  False
  it "e3" $
    antisimetrica' (R ([1,2],[(1,1),(2,1)]))  `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG antisimetrica
  describe "def. 2" $ specG antisimetrica2
  describe "def. 3" $ specG antisimetrica3
  describe "def. 4" $ specG antisimetrica4
  describe "def. 5" $ specG antisimetrica5
  describe "equivalencia" $ it "p1" $ property prop_antisimetrica
