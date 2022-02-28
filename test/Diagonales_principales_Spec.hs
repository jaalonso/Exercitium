module Diagonales_principales_Spec (main, spec) where

import Diagonales_principales
import Data.Array
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Array (Int,Int) Int -> [[Int]]) -> Spec
specG diagonalesPrincipales = do
  it "e1" $
    diagonalesPrincipales (listArray ((1,1),(3,4)) [1..12])
    `shouldBe` [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
  it "e2" $
    diagonalesPrincipales (listArray ((1,1),(1,1)) [3])
    `shouldBe` [[3]]
  it "e3" $
    diagonalesPrincipales (listArray ((1,1),(1,4)) [1..4])
    `shouldBe` [[1],[2],[3],[4]]
  it "e3" $
    diagonalesPrincipales (listArray ((1,1),(4,1)) [1..4])
    `shouldBe` [[4],[3],[2],[1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG diagonalesPrincipales1
  describe "def. 2" $ specG diagonalesPrincipales2
