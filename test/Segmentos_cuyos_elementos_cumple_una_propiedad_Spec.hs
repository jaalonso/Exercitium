module Segmentos_cuyos_elementos_cumple_una_propiedad_Spec (main, spec) where

import Segmentos_cuyos_elementos_cumple_una_propiedad
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> [Int] -> [[Int]]) -> Spec
specG segmentos = do
  it "e1" $
    segmentos even [1,2,0,4,9,6,4,5,7,2]  `shouldBe`  [[2,0,4],[6,4],[2]]
  it "e2" $
    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  `shouldBe`  [[1],[9],[5,7]]

spec :: Spec
spec = do
  describe "def. 1" $ specG segmentos1
  describe "def. 2" $ specG segmentos2
  describe "def. 3" $ specG segmentos3
