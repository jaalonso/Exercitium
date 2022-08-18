module Permutacion_ciclica_Spec (main, spec) where

import Permutacion_ciclica
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    ciclo [2,5,7,9]  `shouldBe` [9,2,5,7]
  it "e2" $
    ciclo ([] :: [Int]) `shouldBe` []
  it "e3" $
    ciclo [2]        `shouldBe` [2]
