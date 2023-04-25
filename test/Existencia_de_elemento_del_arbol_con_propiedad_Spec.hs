module Existencia_de_elemento_del_arbol_con_propiedad_Spec (main, spec) where

import Existencia_de_elemento_del_arbol_con_propiedad
import Arboles_binarios (Arbol (H, N))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    algunoArbol (N 5 (N 3 (H 1) (H 4)) (H 2)) (>4)  `shouldBe`  True
  it "e2" $
    algunoArbol (N 5 (N 3 (H 1) (H 4)) (H 2)) (>7)  `shouldBe`  False
