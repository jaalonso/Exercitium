module Mastermind_Spec (main, spec) where

import Mastermind
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    mastermind [3,3] [3,2]          `shouldBe`  (1,0)
  it "e2" $
    mastermind [3,5,3] [3,2,5]      `shouldBe`  (1,1)
  it "e3" $
    mastermind [3,5,3,2] [3,2,5,3]  `shouldBe`  (1,3)
  it "e4" $
    mastermind [3,5,3,3] [3,2,5,3]  `shouldBe`  (2,1)
  it "p5" $
    property prop_mastermind
