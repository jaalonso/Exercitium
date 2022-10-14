module Suma_elementos_consecutivos_Spec (main, spec) where

import Suma_elementos_consecutivos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer] -> [Integer]) -> Spec
specG sumaConsecutivos = do
  it "e1" $
    sumaConsecutivos [3,1,5,2]  `shouldBe`  [4,6,7]
  it "e2" $
    sumaConsecutivos [3]        `shouldBe`  []

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaConsecutivos1
  describe "def. 2" $ specG sumaConsecutivos2
  describe "def. 3" $ specG sumaConsecutivos3
  describe "def. 4" $ specG sumaConsecutivos4
  describe "equivalencia" $ it "p1" $ property prop_sumaConsecutivos
