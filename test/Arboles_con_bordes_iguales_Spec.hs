module Arboles_con_bordes_iguales_Spec (main, spec) where

import Arboles_con_bordes_iguales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    igualBorde arbol1 arbol2  `shouldBe`  True
  it "e2" $
    igualBorde arbol1 arbol3  `shouldBe`  False
  it "e3" $
    igualBorde arbol1 arbol4  `shouldBe`  False
