module Numero_de_hojas_de_un_arbol_binario_Spec (main, spec) where

import Numero_de_hojas_de_un_arbol_binario
import Arboles_binarios
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` 3
  it "e2" $
    nNodos (N 9 (N 3 (H 2) (H 4)) (H 7)) `shouldBe` 2
