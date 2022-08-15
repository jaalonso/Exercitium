module Distancia_entre_dos_puntos_Spec (main, spec) where

import Distancia_entre_dos_puntos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    distancia (1,2) (4,6)  `shouldBe`  5.0
