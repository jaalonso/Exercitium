module Numero_de_raices_de_la_ecuacion_de_segundo_grado_Spec (main, spec) where

import Numero_de_raices_de_la_ecuacion_de_segundo_grado
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    numeroDeRaices 2 0 3    `shouldBe`  0
  it "e2" $
    numeroDeRaices 4 4 1    `shouldBe`  1
  it "e3" $
    numeroDeRaices 5 23 12  `shouldBe`  2
