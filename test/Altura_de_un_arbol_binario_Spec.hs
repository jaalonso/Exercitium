module Altura_de_un_arbol_binario_Spec (main, spec) where

import Altura_de_un_arbol_binario
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    altura (Hoja 1)
    `shouldBe` 0
  it "e2" $
    altura (Nodo (Hoja 1) (Hoja 6))
    `shouldBe` 1
  it "e3" $
    altura (Nodo (Nodo (Hoja 1) (Hoja 6)) (Hoja 2))
    `shouldBe` 2
  it "e4" $
    altura (Nodo (Nodo (Hoja 1) (Hoja 6)) (Nodo (Hoja 2) (Hoja 7)))
    `shouldBe` 2
