module Imagen_especular_de_un_arbol_binario_Spec (main, spec) where

import Imagen_especular_de_un_arbol_binario
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` N 9 (H 7) (N 3 (H 4) (H 2))
