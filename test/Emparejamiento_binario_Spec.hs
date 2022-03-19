module Emparejamiento_binario_Spec (main, spec) where

import Emparejamiento_binario
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int -> Int -> Int] -> [Int] -> [Int] -> [Int]) -> Spec
specG zipBinario = do
  it "e1" $ zipBinario [(+), (*), (*)] [2,2,2] [4,4,4]    `shouldBe` [6,8,8]
  it "e2" $ zipBinario [(+)] [2,2,2] [4,4,4]              `shouldBe` [6]

spec :: Spec
spec = do
  describe "def. 1" $ specG zipBinario1
  describe "def. 2" $ specG zipBinario2
  describe "def. 3" $ specG zipBinario3
  describe "def. 4" $ specG zipBinario4
