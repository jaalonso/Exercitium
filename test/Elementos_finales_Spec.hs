module Elementos_finales_Spec (main, spec) where

import Elementos_finales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    finales1 3 [2,5,4,7,9,6]  `shouldBe`  [7,9,6]
  it "e2" $
    finales2 3 [2,5,4,7,9,6]  `shouldBe`  [7,9,6]
