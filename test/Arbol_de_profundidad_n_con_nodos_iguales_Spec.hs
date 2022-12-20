module Arbol_de_profundidad_n_con_nodos_iguales_Spec (main, spec) where

import Arbol_de_profundidad_n_con_nodos_iguales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    replicateArbol 0 5  `shouldBe`  H 5
  it "e2" $
    replicateArbol 1 5  `shouldBe`  N 5 (H 5) (H 5)
  it "e3" $
    replicateArbol 2 5  `shouldBe`  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
