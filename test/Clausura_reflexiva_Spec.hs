module Clausura_reflexiva_Spec (main, spec) where

import Clausura_reflexiva
import Relaciones_binarias
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    show (clausuraReflexiva (R ([1,3],[(1,1),(3,1)])))
    `shouldBe` "R ([1,3],[(1,1),(3,1),(3,3)])"
