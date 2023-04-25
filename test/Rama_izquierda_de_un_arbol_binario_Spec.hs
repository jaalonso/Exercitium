module Rama_izquierda_de_un_arbol_binario_Spec (main, spec) where

import Rama_izquierda_de_un_arbol_binario
import Arbol_binario_valores_en_nodos (Arbol (H, N))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    ramaIzquierda (N 2 (N 5 (N 3 H H) (N 7 H H)) (N 4 H H))
    `shouldBe` [2,5,3]
