module Enumera_arbol_Spec (main, spec) where

import Enumera_arbol
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Arbol String -> Arbol Int) -> Spec
specG enumeraArbol = do
  it "e1" $
    enumeraArbol ej1
    `shouldBe` N (N (H 0) 1 (H 2)) 3 (N (H 4) 5 (H 6))

spec :: Spec
spec = do
  describe "def. 1" $ specG enumeraArbol1
  describe "def. 2" $ specG enumeraArbol2
  describe "def. 3" $ specG enumeraArbol3
  describe "def. 4" $ specG enumeraArbol4
  describe "equivalencia" $ it "p1" $ property prop_enumeraArbol
