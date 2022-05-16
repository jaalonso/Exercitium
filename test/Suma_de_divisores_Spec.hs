module Suma_de_divisores_Spec (main, spec) where

import Suma_de_divisores
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG sumaDivisores = do
  it "e1" $
    sumaDivisores 12  `shouldBe`  28
  it "e2" $
    sumaDivisores 25  `shouldBe`  31

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaDivisores1
  describe "def. 2" $ specG sumaDivisores2
  describe "def. 3" $ specG sumaDivisores3
  describe "equivalencia" $ it "p1" $ property prop_sumaDivisores
