module Pertenencia_de_un_elemento_a_un_arbol_Spec (main, spec) where

import Pertenencia_de_un_elemento_a_un_arbol
import Arboles_binarios (Arbol (..))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    pertenece 4 (N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))) `shouldBe` True
  it "e2" $
    pertenece 0 (N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))) `shouldBe` False
