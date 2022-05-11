module Particiones_de_enteros_positivos_Spec (main, spec) where

import Particiones_de_enteros_positivos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> [[Int]]) -> Spec
specG particiones = do
  it "e1" $
    particiones 4 `shouldBe` [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
  it "e2" $
    particiones 5 `shouldBe` [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG particiones1
  describe "def. 2" $ specG particiones2
  describe "def. 3" $ specG particiones3
  describe "def. 4" $ specG particiones4
