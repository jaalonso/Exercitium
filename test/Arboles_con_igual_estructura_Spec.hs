module Arboles_con_igual_estructura_Spec (main, spec) where

import Arboles_con_igual_estructura
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    igualEstructura ej3arbol1 ej3arbol2 `shouldBe` True
  it "e2" $
    igualEstructura ej3arbol1 ej3arbol3 `shouldBe` False
  it "e3" $
    igualEstructura ej3arbol1 ej3arbol4 `shouldBe` False
