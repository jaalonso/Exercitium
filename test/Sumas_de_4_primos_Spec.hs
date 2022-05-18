module Sumas_de_4_primos_Spec (main, spec) where

import Sumas_de_4_primos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [(Integer,Integer,Integer,Integer)]) -> Spec
specG suma4primos = do
  it "e1" $
    suma4primos 18 `shouldBe` [(2,2,3,11),(2,2,7,7),(3,3,5,7),(3,5,5,5)]

spec :: Spec
spec = do
  describe "def. 1" $ specG suma4primos1
  describe "def. 2" $ specG suma4primos2
  describe "equivalencia" $ it "p1" $ property prop_suma4primos
