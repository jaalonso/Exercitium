module Parejas_de_numeros_y_divisores_Spec (main, spec) where

import Parejas_de_numeros_y_divisores
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> [(Int,Int)]) -> Spec
specG divisoresHasta = do
  it "e1" $
    divisoresHasta 6
    `shouldBe` [(2,1),(3,1),(4,1),(5,1),(6,1),(4,2),(6,2),(6,3)]
  it "e1" $
    divisoresHasta 8
    `shouldBe` [(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(4,2),(6,2),(8,2),(6,3),(8,4)]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisoresHasta1
  describe "def. 2" $ specG divisoresHasta2
  describe "def. 3" $ specG divisoresHasta3
  describe "def. 4" $ specG divisoresHasta4
  describe "equivalencia" $ it "p1" $ property prop_divisoresHasta
