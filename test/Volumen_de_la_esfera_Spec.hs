module Volumen_de_la_esfera_Spec (main, spec) where

import Volumen_de_la_esfera
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    volumenEsfera 10 `shouldBe` 4188.7905
