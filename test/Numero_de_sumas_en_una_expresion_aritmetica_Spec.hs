module Numero_de_sumas_en_una_expresion_aritmetica_Spec (main, spec) where

import Numero_de_sumas_en_una_expresion_aritmetica
import Expresion_aritmetica_con_variables (Expr (C, V, S, P))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    sumas (P (V 'z') (S (C 3) (V 'x')))  `shouldBe`  1
  it "e2" $
    sumas (S (V 'z') (S (C 3) (V 'x')))  `shouldBe`  2
  it "e3" $
    sumas (P (V 'z') (P (C 3) (V 'x')))  `shouldBe`  0
