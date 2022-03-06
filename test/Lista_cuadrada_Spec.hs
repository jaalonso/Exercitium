module Lista_cuadrada_Spec (main, spec) where

import Lista_cuadrada
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> Int -> [Int] -> [[Int]])  -> Spec
specG listaCuadrada = do
  it "e1" $
    listaCuadrada 3 7 [0,3,5,2,4]  `shouldBe`  [[0,3,5],[2,4,7],[7,7,7]]
  it "e2" $
    listaCuadrada 3 7 [0..]        `shouldBe`  [[0,1,2],[3,4,5],[6,7,8]]

spec :: Spec
spec = do
  describe "def. 1" $ specG listaCuadrada1
  describe "def. 2" $ specG listaCuadrada2
  describe "def. 3" $ specG listaCuadrada3
  describe "def. 4" $ specG listaCuadrada4
