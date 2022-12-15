module Profundidad_de_un_arbol_binario_Spec (main, spec) where

import Profundidad_de_un_arbol_binario
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              `shouldBe`  2
  it "e2" $
    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  `shouldBe`  3
  it "e3" $
    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  `shouldBe`  2
