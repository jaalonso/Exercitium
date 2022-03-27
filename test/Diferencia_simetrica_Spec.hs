module Diferencia_simetrica_Spec (main, spec) where

import Diferencia_simetrica
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> [Int]) -> Spec
specG diferenciaSimetrica = do
  it "e1" $
    diferenciaSimetrica [2,5,3] [4,2,3,7]    `shouldBe`  [4,5,7]
  it "e2" $
    diferenciaSimetrica [2,5,3] [5,2,3]      `shouldBe`  []
  it "e3" $
    diferenciaSimetrica [2,5,2] [4,2,3,7]    `shouldBe`  [3,4,5,7]
  it "e4" $
    diferenciaSimetrica [2,5,2] [4,2,4,7]    `shouldBe`  [4,5,7]
  it "e5" $
    diferenciaSimetrica [2,5,2,4] [4,2,4,7]  `shouldBe`  [5,7]

spec :: Spec
spec = do
  describe "def. 1" $ specG diferenciaSimetrica1
  describe "def. 2" $ specG diferenciaSimetrica2
  describe "def. 3" $ specG diferenciaSimetrica3
  describe "def. 4" $ specG diferenciaSimetrica4
  describe "def. 5" $ specG diferenciaSimetrica5
