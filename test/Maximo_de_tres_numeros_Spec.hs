module Maximo_de_tres_numeros_Spec (main, spec) where

import Maximo_de_tres_numeros
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    maxTres 6 2 4  `shouldBe`  6
  it "e2" $
    maxTres 6 7 4  `shouldBe`  7
  it "e3" $
    maxTres 6 7 9  `shouldBe`  9
