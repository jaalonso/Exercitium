module Valor_de_una_expresion_aritmetica_basica_Spec (main, spec) where

import Valor_de_una_expresion_aritmetica_basica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    valor (P (C 2) (S (C 3) (C 7))) `shouldBe`  20
