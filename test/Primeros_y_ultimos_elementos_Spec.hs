module Primeros_y_ultimos_elementos_Spec (main, spec) where

import Primeros_y_ultimos_elementos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  `shouldBe`  [2,6,7,9,2,3]
