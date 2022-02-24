module Caminos_en_un_triangulo_Spec (main, spec) where

import Caminos_en_un_triangulo
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([[Int]] -> [[Int]]) -> Spec
specG caminos' = do
  it "e1" $
    caminos' [[3],[7,4]] `shouldBe`
    [[3,7],[3,4]]
  it "e2" $
    caminos' [[3],[7,4],[2,4,6]] `shouldBe`
    [[3,7,2],[3,7,4],[3,4,4],[3,4,6]]
  it "e3" $
    caminos' [[3],[7,4],[2,4,6],[8,5,9,3]] `shouldBe`
    [[3,7,2,8],[3,7,2,5],[3,7,4,5],[3,7,4,9],[3,4,4,5],[3,4,4,9],[3,4,6,9],[3,4,6,3]]


spec :: Spec
spec = do
  describe "def. 1" $ specG caminos
