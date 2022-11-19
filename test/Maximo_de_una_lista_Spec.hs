module Maximo_de_una_lista_Spec (main, spec) where

import Maximo_de_una_lista
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> Int) -> Spec
specG maximo =
  it "e1" $
    maximo [3,7,2,5] `shouldBe`  7

spec :: Spec
spec = do
  describe "def. 1" $ specG maximo1
  describe "def. 2" $ specG maximo2
  describe "def. 3" $ specG maximo3
  describe "def. 4" $ specG maximo4
  describe "equivalencia" $ it "p1" $ property prop_maximo
