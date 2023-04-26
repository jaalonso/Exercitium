module Valor_de_una_expresion_aritmetica_con_variables_Spec (main, spec) where

import Valor_de_una_expresion_aritmetica_con_variables
import Expresion_aritmetica_con_variables (Expr (C, V, S, P))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    valor (P (C 2) (S (V 'a') (V 'b'))) [('a',2),('b',5)]
    `shouldBe` 14
