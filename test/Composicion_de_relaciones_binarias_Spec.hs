module Composicion_de_relaciones_binarias_Spec (main, spec) where

import Composicion_de_relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]) -> Spec
specG composicion = do
  it "e1" $
    composicion [(1,2)] [(2,3),(2,4)] `shouldBe`
    [(1,3),(1,4)]
  it "e2" $
    composicion [(1,2),(5,2)] [(2,3),(2,4)] `shouldBe`
    [(1,3),(1,4),(5,3),(5,4)]
  it "e3" $
    composicion [(1,2),(1,4),(1,5)] [(2,3),(4,3)] `shouldBe`
    [(1,3)]

spec :: Spec
spec = do
  describe "def. 1" $ specG composicion1
  describe "def. 2" $ specG composicion2
  describe "equivalencia" $ it "p1" $ property prop_composicion
