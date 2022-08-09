module Elementos_finales_Spec (main, spec) where

import Elementos_finales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    finales 3 [2,5,4,7,9,6]  `shouldBe`  [7,9,6]
