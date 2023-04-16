module Aplicacion_de_una_funcion_a_una_expresion_aritmetica_Spec (main, spec) where

import Aplicacion_de_una_funcion_a_una_expresion_aritmetica
import Expresion_aritmetica_basica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    aplica (+2) (S (P (C 3) (C 5)) (P (C 6) (C 7)))
    `shouldBe` S (P (C 5) (C 7)) (P (C 8) (C 9))
  it "e2" $
    aplica (*2) (S (P (C 3) (C 5)) (P (C 6) (C 7)))
    `shouldBe` S (P (C 6) (C 10)) (P (C 12) (C 14))
