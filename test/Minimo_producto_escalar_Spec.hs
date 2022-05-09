module Minimo_producto_escalar_Spec (main, spec) where

import Minimo_producto_escalar
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer] -> [Integer] -> Integer) -> Spec
specG menorProductoEscalar = do
  it "e1" $
    menorProductoEscalar [3,2,5]  [1,4,6]  `shouldBe` 29
  it "e2" $
    menorProductoEscalar [3,2,5]  [1,4,-6] `shouldBe` -19
  it "e3" $
    menorProductoEscalar [0..5]   [0..5]   `shouldBe` 20

spec :: Spec
spec = do
  describe "def. 1" $ specG menorProductoEscalar1
  describe "def. 2" $ specG menorProductoEscalar2
  describe "def. 3" $ specG menorProductoEscalar3
