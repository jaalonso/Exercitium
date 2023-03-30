module Clausura_simetrica_Spec (main, spec) where

import Clausura_simetrica
import Relaciones_binarias
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (clausuraSimetrica (R ([1,3,5],[(1,1),(3,1),(1,5)])))
    `shouldBe` "R ([1,3,5],[(1,1),(3,1),(1,5),(1,3),(5,1)])"
