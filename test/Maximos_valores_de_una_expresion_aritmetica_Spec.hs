module Maximos_valores_de_una_expresion_aritmetica_Spec (main, spec) where

import Maximos_valores_de_una_expresion_aritmetica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    maximo (E (S (C 10) (P (R (C 1) X) X)) 2) [-3..3]
    `shouldBe` (100,[0,1])
