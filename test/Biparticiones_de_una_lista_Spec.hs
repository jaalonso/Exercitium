module Biparticiones_de_una_lista_Spec (main, spec) where

import Biparticiones_de_una_lista
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> [([Int],[Int])]) -> Spec
specG biparticiones = do
  it "e1" $
    biparticiones [3,2,5]
    `shouldBe` [([],[3,2,5]),([3],[2,5]),([3,2],[5]),([3,2,5],[])]

spec :: Spec
spec = do
  describe "def. 1" $ specG biparticiones1
  describe "def. 2" $ specG biparticiones2
  describe "def. 3" $ specG biparticiones3
  describe "def. 4" $ specG biparticiones4
  describe "def. 5" $ specG biparticiones5
  describe "def. 6" $ specG biparticiones6
