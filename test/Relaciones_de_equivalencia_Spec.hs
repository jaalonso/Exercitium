module Relaciones_de_equivalencia_Spec (main, spec) where

import Relaciones_de_equivalencia
import Relaciones_binarias
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    esEquivalencia (R ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)]))
    `shouldBe` True
  it "e2" $
    esEquivalencia (R ([1,2,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)]))
    `shouldBe` False
  it "e3" $
    esEquivalencia (R ([1,3,5],[(1,1),(1,3),(3,3),(5,5)]))
    `shouldBe` False
