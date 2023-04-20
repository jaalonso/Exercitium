module Valor_de_una_expresion_aritmetica_Spec (main, spec) where

import Valor_de_una_expresion_aritmetica
import Tipo_expresion_aritmetica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    valor (Op (Suma (Lit 3) (Lit 5)))     `shouldBe`  -8
  it "e2" $
    valor (SiCero (Lit 0) (Lit 4) (Lit 5)) `shouldBe` 4
  it "e3" $
    valor (SiCero (Lit 1) (Lit 4) (Lit 5)) `shouldBe` 5
