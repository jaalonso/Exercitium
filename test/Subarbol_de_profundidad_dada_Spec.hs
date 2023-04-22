module Subarbol_de_profundidad_dada_Spec (main, spec) where

import Subarbol_de_profundidad_dada
import Arboles_binarios
import Test.Hspec

main :: IO ()
main = hspec spec

spec ::Spec
spec  = do
  it "e1" $
    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` H 9
  it "e2" $
    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` N 9 (H 3) (H 7)
  it "e3" $
    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` N 9 (N 3 (H 2) (H 4)) (H 7)
  it "e4" $
    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` N 9 (N 3 (H 2) (H 4)) (H 7)
