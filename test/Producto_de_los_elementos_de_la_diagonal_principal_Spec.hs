module Producto_de_los_elementos_de_la_diagonal_principal_Spec (main, spec) where

import Producto_de_los_elementos_de_la_diagonal_principal
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([[Integer]] -> Integer) -> Spec
specG productoDiagonal = do
  it "e1" $
    productoDiagonal [[1]]                     `shouldBe` 1
  it "e2" $
    productoDiagonal [[1,2,3],[4,5,6],[7,8,9]]  `shouldBe` 1 * 5 * 9
  it "e3" $
    productoDiagonal [[3,5,2],[4,7,1],[6,9,0]]  `shouldBe` 3 * 7 * 0
  it "e4" $
    productoDiagonal [[3,5],[4,7],[6,9]]        `shouldBe` 3 * 7
  it "e5" $
    productoDiagonal [[3,5,2],[4,7,1]]          `shouldBe` 3 * 7

spec :: Spec
spec = do
  describe "def. 1" $ specG productoDiagonal1
  describe "def. 2" $ specG productoDiagonal2
  describe "def. 3" $ specG productoDiagonal3
  describe "def. 4" $ specG productoDiagonal4
  describe "def. 5" $ specG productoDiagonal5
