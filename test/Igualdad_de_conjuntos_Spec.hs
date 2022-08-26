module Igualdad_de_conjuntos_Spec (main, spec) where

import Igualdad_de_conjuntos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> Bool) -> Spec
specG iguales = do
  it "e1" $
    iguales [3,2,3] [2,3]    `shouldBe`  True
  it "e2" $
    iguales [3,2,3] [2,3,2]  `shouldBe`  True
  it "e3" $
    iguales [3,2,3] [2,3,4]  `shouldBe`  False
  it "e4" $
    iguales [2,3] [4,5]      `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG iguales1
  describe "def. 2" $ specG iguales2
  describe "def. 3" $ specG iguales3
  describe "equivalencia" $ it "p1" $ property prop_iguales
