module Descomposiciones_triangulares_Spec (main, spec) where

import Descomposiciones_triangulares
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> [(Int, Int, Int)]) -> Spec
specG descomposicionesTriangulares = do
  it "e1" $
    descomposicionesTriangulares  4 `shouldBe` []
  it "e2" $
    descomposicionesTriangulares  5 `shouldBe` [(1,1,3)]
  it "e3" $
    descomposicionesTriangulares 12 `shouldBe` [(1,1,10),(3,3,6)]
  it "e4" $
    descomposicionesTriangulares 30 `shouldBe` [(1,1,28),(3,6,21),(10,10,10)]
  it "e5" $
    descomposicionesTriangulares 61 `shouldBe` [(1,15,45),(3,3,55),(6,10,45),(10,15,36)]
  it "e6" $
    descomposicionesTriangulares 52 `shouldBe` [(1,6,45),(1,15,36),(3,21,28),(6,10,36),(10,21,21)]
  it "e7" $
    descomposicionesTriangulares 82 `shouldBe` [(1,3,78),(1,15,66),(1,36,45),(6,10,66),(6,21,55),(10,36,36)]

spec :: Spec
spec = do
  describe "def. 1" $ specG descomposicionesTriangulares1
  describe "def. 2" $ specG descomposicionesTriangulares2
  describe "def. 3" $ specG descomposicionesTriangulares3
  describe "def. 4" $ specG descomposicionesTriangulares4
  describe "equivalencia" $ it "p1" $ property prop_descomposicionesTriangulares
