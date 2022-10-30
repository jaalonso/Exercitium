module Subconjuntos_de_un_conjunto_Spec (main, spec) where

import Subconjuntos_de_un_conjunto
import Data.List (sort)
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> [[Int]]) -> Spec
specG subconjuntos = do
  it "e1" $
    sort (subconjuntos [2,3,4]) `shouldBe`
    [[],[2],[2,3],[2,3,4],[2,4],[3],[3,4],[4]]
  it "e2" $
    sort (subconjuntos [1,2,3,4]) `shouldBe`
    [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,4],[1,3],[1,3,4],[1,4],[2],[2,3],[2,3,4],[2,4],[3],[3,4],[4]]

spec :: Spec
spec = do
  describe "def. 1" $ specG subconjuntos1
  describe "def. 2" $ specG subconjuntos2
  describe "def. 3" $ specG subconjuntos3
