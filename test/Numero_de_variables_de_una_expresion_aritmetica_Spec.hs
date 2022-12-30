module Numero_de_variables_de_una_expresion_aritmetica_Spec (main, spec) where

import Numero_de_variables_de_una_expresion_aritmetica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    numVars (C 3)               `shouldBe`  0
  it "e2" $
    numVars X                   `shouldBe`  1
  it "e3" $
    numVars (P X (S (C 13) X))  `shouldBe`  2
