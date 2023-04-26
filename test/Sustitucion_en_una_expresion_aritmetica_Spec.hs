module Sustitucion_en_una_expresion_aritmetica_Spec (main, spec) where

import Sustitucion_en_una_expresion_aritmetica
import Expresion_aritmetica_con_variables (Expr (C, V, S, P))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    sustitucion (P (V 'z') (S (C 3) (V 'x'))) [('x',7),('z',9)]
    `shouldBe` P (C 9) (S (C 3) (C 7))
  it "e2" $
    sustitucion (P (V 'z') (S (C 3) (V 'y'))) [('x',7),('z',9)]
    `shouldBe` P (C 9) (S (C 3) (V 'y'))
