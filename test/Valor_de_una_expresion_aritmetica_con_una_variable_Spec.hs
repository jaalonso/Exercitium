module Valor_de_una_expresion_aritmetica_con_una_variable_Spec (main, spec) where

import Valor_de_una_expresion_aritmetica_con_una_variable
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    valor (P X (S (C 13) X)) 2  `shouldBe`  30
