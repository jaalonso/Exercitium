module Elemento_mediano_Spec (main, spec) where

import Elemento_mediano
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    mediano 3 2 5  `shouldBe`  3
  it "e2" $
    mediano 2 4 5  `shouldBe`  4
  it "e3" $
    mediano 2 6 5  `shouldBe`  5
  it "e4" $
    mediano 2 6 6  `shouldBe`  6
