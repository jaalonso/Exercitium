module Arbol_con_las_hojas_en_la_profundidad_dada_Spec (main, spec) where

import Arbol_con_las_hojas_en_la_profundidad_dada
import Arbol_binario_valores_en_hojas (Arbol (..))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    creaArbol 2 `shouldBe`
    Nodo (Nodo (Hoja ()) (Hoja ())) (Nodo (Hoja ()) (Hoja ()))
