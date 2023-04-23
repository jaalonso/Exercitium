module Arboles_con_la_misma_forma_Spec (main, spec) where

import Arboles_con_la_misma_forma
import Arbol_binario_valores_en_hojas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Arbol Int -> Arbol Int -> Bool) -> Spec
specG mismaForma = do
  it "e1" $
    mismaForma arbol1 arbol2 `shouldBe` True
  it "e2" $
    mismaForma arbol1 arbol3 `shouldBe` False
  it "e3" $
    mismaForma arbol3 arbol4 `shouldBe` True
  where
    arbol1 = Hoja 5
    arbol2 = Hoja 3
    arbol3 = Nodo (Hoja 6) (Hoja 7)
    arbol4 = Nodo (Hoja 9) (Hoja 5)

spec :: Spec
spec = do
  describe "def. 1" $ specG mismaForma1
  describe "def. 2" $ specG mismaForma2
  describe "equivalencia" $ it "p1" $ property prop_mismaForma
