module Numero_de_operaciones_en_una_expresion_Spec (main, spec) where

import Numero_de_operaciones_en_una_expresion
import Tipo_expresion_aritmetica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    numeroOps (Lit 3)                      `shouldBe`  0
  it "e2" $
    numeroOps (Suma (Lit 7) (Op (Lit 5)))  `shouldBe`  2
