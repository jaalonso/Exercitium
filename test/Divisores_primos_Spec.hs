module Divisores_primos_Spec (main, spec) where

import Divisores_primos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG divisoresPrimos = do
  it "e1" $
    divisoresPrimos 40 `shouldBe` [2,5]
  it "e2" $
    divisoresPrimos 70 `shouldBe` [2,5,7]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisoresPrimos1
  describe "def. 2" $ specG divisoresPrimos2
  describe "def. 3" $ specG divisoresPrimos3
  describe "def. 4" $ specG divisoresPrimos4
  describe "equivalencia" $ it "p1" $ property prop_divisoresPrimos
