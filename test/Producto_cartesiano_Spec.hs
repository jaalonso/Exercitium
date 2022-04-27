module Producto_cartesiano_Spec (main, spec) where

import Producto_cartesiano
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG producto = do
  it "e1" $
    producto [[1,3],[2,5]]
    `shouldBe` [[1,2],[1,5],[3,2],[3,5]]
  it "e2" $
    producto [[1,3],[2,5],[6,4]]
    `shouldBe` [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
  it "e3" $
    producto [[1,3,5],[2,4]]
    `shouldBe` [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]

spec :: Spec
spec = do
  describe "def. 1" $ specG producto1
  describe "def. 2" $ specG producto2
  describe "def. 3" $ specG producto3
  describe "def. 4" $ specG producto4
  describe "def. 5" $ specG producto5
  describe "def. 6" $ specG producto6
  describe "def. 7" $ specG producto7
  describe "def. 8" $ specG producto8
  describe "def. 9" $ specG producto9
  describe "def. 9" $ specG producto10
  describe "def. 9" $ specG producto11
  describe "def. 9" $ specG producto12
  describe "equivalencia" $ modifyMaxSize (const 7) $ it "p1" $ property prop_producto
