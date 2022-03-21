module Pares_adyacentes_iguales_Spec (main, spec) where

import Pares_adyacentes_iguales
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([[Int]] -> Int) -> Spec
specG numeroParesAdyacentesIguales = do
  it "e1" $
    numeroParesAdyacentesIguales [[0,1],[0,2]]              `shouldBe`  1
  it "e2" $
    numeroParesAdyacentesIguales [[0,0],[1,2]]              `shouldBe`  1
  it "e3" $
    numeroParesAdyacentesIguales [[0,1],[0,0]]              `shouldBe`  2
  it "e4" $
    numeroParesAdyacentesIguales [[1,2],[1,4],[4,4]]        `shouldBe`  3
  it "e5" $
    numeroParesAdyacentesIguales [[0,0,0],[0,0,0],[0,0,0]]  `shouldBe`  12
  it "e6" $
    numeroParesAdyacentesIguales [[0,0,0],[0,1,0],[0,0,0]]  `shouldBe`  8

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroParesAdyacentesIguales1
  describe "def. 2" $ specG numeroParesAdyacentesIguales2
  describe "def. 3" $ specG numeroParesAdyacentesIguales3
  describe "def. 4" $ specG numeroParesAdyacentesIguales4
