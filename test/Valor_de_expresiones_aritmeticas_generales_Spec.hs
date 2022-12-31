module Valor_de_expresiones_aritmeticas_generales_Spec (main, spec) where

import Valor_de_expresiones_aritmeticas_generales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    valor (A S (A R (C 7) (C 3)) (A M (C 2) (C 5)))  `shouldBe`  14
  it "e2" $
    valor (A M (A R (C 7) (C 3)) (A S (C 2) (C 5)))  `shouldBe`  28
