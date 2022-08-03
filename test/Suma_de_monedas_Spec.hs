module Suma_de_monedas_Spec (main, spec) where

import Suma_de_monedas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    sumaMonedas 0 0 0 0 1  `shouldBe`  20
  it "e2" $
    sumaMonedas 0 0 8 0 3  `shouldBe` 100
  it "e3" $
    sumaMonedas 1 1 1 1 1  `shouldBe`  38
