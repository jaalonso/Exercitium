module Aplicacion_de_una_funcion_a_un_arbol_Spec (main, spec) where

import Aplicacion_de_una_funcion_a_un_arbol
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    mapArbol (+ 1) (Nodo (Hoja 2) (Hoja 4))
    `shouldBe` Nodo (Hoja 3) (Hoja 5)
