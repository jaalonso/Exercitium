module Rango_de_una_lista_Spec (main, spec) where

import Rango_de_una_lista
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    rango [3,2,7,5]  `shouldBe`  [2,7]
