module Clausura_Spec (main, spec) where

import Clausura
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Int) -> [Int] -> [Int]) -> Spec
specG clausura = do
  it "e1" $
    clausura (\x -> -x) [0,1,2]         `shouldBe`  [-2,-1,0,1,2]
  it "e2" $
    clausura (\x -> (x+1) `mod` 5) [0]  `shouldBe`  [0,1,2,3,4]

spec :: Spec
spec = do
  describe "def. 1" $ specG clausura1
  describe "def. 2" $ specG clausura2
  describe "def. 3" $ specG clausura3
  describe "def. 4" $ specG clausura4
