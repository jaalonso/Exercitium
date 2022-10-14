module Producto_escalar_Spec (main, spec) where

import Producto_escalar
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer] -> [Integer] -> Integer) -> Spec
specG productoEscalar =
  it "e1" $
    productoEscalar [1,2,3] [4,5,6]  `shouldBe`  32

spec :: Spec
spec = do
  describe "def. 1" $ specG productoEscalar1
  describe "def. 2" $ specG productoEscalar2
  describe "def. 3" $ specG productoEscalar3
  describe "def. 4" $ specG productoEscalar4
  describe "def. 5" $ specG productoEscalar5
  describe "def. 5" $ specG productoEscalar6
  describe "equivalencia" $ it "p1" $ property prop_productoEscalar
