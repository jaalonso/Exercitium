module Tres_diferentes_Spec (main, spec) where

import Tres_diferentes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    tresDiferentes 3 5 2  `shouldBe`  True
  it "e2" $
    tresDiferentes 3 5 3  `shouldBe`  False
