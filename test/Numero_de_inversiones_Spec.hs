module Numero_de_inversiones_Spec (main, spec) where

import Numero_de_inversiones
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> Int) -> Spec
specG numeroInversiones = do
  it "e1" $
    numeroInversiones [2,1,4,3]  `shouldBe`  2
  it "e2" $
    numeroInversiones [4,3,1,2]  `shouldBe`  5

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroInversiones1
  describe "def. 2" $ specG numeroInversiones2
  describe "def. 3" $ specG numeroInversiones3
  describe "def. 4" $ specG numeroInversiones4
  describe "equivalencia" $ it "p1" $ property prop_numeroInversiones
