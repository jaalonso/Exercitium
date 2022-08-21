module Raices_de_la_ecuacion_de_segundo_grado_Spec (main, spec) where

import Raices_de_la_ecuacion_de_segundo_grado
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    raices 1 3 2    `shouldBe`  [-1.0,-2.0]
  it "e2" $
    raices 1 (-2) 1 `shouldBe`  [1.0,1.0]
  it "e3" $
    raices 1 0 1    `shouldBe`  []
