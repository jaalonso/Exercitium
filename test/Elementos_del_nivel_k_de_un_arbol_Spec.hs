module Elementos_del_nivel_k_de_un_arbol_Spec (main, spec) where

import Elementos_del_nivel_k_de_un_arbol
import Arboles_binarios (Arbol (H, N))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    nivel 0 (N 7 (N 2 (H 5) (H 4)) (H 9))  `shouldBe`  [7]
  it "e2" $
    nivel 1 (N 7 (N 2 (H 5) (H 4)) (H 9))  `shouldBe`  [2,9]
  it "e3" $
    nivel 2 (N 7 (N 2 (H 5) (H 4)) (H 9))  `shouldBe`  [5,4]
  it "e4" $
    nivel 3 (N 7 (N 2 (H 5) (H 4)) (H 9))  `shouldBe`  []
