module Matriz_zigzagueante_Spec (main, spec) where

import Matriz_zigzagueante
import Data.Matrix
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> Matrix Int) -> Spec
specG zigZag = do
  it "e1" $
    toList (zigZag 3) `shouldBe`
    [0,1,5,2,4,6,3,7,8]
  it "e2" $
    toList (zigZag 4) `shouldBe`
    [0,1,5,6,2,4,7,12,3,8,11,13,9,10,14,15]
  it "e3" $
    toList (zigZag 5) `shouldBe`
    [0,1,5,6,14,2,4,7,13,15,3,8,12,16,21,9,11,17,20,22,10,18,19,23,24]

spec :: Spec
spec = do
  describe "def. 1" $ specG zigZag1
  describe "def. 2" $ specG zigZag2
