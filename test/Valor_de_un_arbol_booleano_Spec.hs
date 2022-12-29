module Valor_de_un_arbol_booleano_Spec (main, spec) where

import Valor_de_un_arbol_booleano
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    valor ej1 `shouldBe` True
  it "e2" $
    valor ej2 `shouldBe` False
