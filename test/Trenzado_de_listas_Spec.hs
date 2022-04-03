module Trenzado_de_listas_Spec (main, spec) where

import Trenzado_de_listas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG trenza = do
  it "e1" $
    trenza [5,1] [2,7,4]             `shouldBe`  [5,2,1,7]
  it "e2" $
    trenza [5,1,7] [2..]             `shouldBe`  [5,2,1,3,7,4]
  it "e3" $
    trenza [2..] [5,1,7]             `shouldBe`  [2,5,3,1,4,7]
  it "e4" $
    take 8 (trenza [2,4..] [1,5..])  `shouldBe`  [2,1,4,5,6,9,8,13]

spec :: Spec
spec = do
  describe "def. 1" $ specG trenza1
  describe "def. 2" $ specG trenza2
  describe "def. 3" $ specG trenza3
  describe "def. 4" $ specG trenza4
