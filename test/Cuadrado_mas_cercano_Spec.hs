module Cuadrado_mas_cercano_Spec (main, spec) where

import Cuadrado_mas_cercano
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG cuadradoCercano = do
  it "e1" $
    cuadradoCercano 2 `shouldBe` 1
  it "e2" $
    cuadradoCercano 6 `shouldBe` 4
  it "e3" $
    cuadradoCercano 8 `shouldBe` 9
  it "p1" $
    property prop_cuadradoCercano

spec :: Spec
spec = do
  describe "def. 1" $ specG cuadradoCercano1
  describe "def. 2" $ specG cuadradoCercano2
  describe "def. 3" $ specG cuadradoCercano3
  describe "def. 4" $ specG cuadradoCercano4
