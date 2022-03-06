module Segmentos_consecutivos_Spec (main, spec) where

import Segmentos_consecutivos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> [[Int]]) -> Spec
specG segmentos = do
  it "e1" $
    segmentos [1,2,5,6,4]     `shouldBe`  [[1,2],[5,6],[4]]
  it "e2" $
    segmentos [1,2,3,4,7,8,9] `shouldBe`  [[1,2,3,4],[7,8,9]]

spec :: Spec
spec = do
  describe "def. 1" $ specG segmentos1
  describe "def. 2" $ specG segmentos2
  describe "def. 3" $ specG segmentos3
