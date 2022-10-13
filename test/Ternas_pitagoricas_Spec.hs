module Ternas_pitagoricas_Spec (main, spec) where

import Ternas_pitagoricas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> [(Int,Int,Int)]) -> Spec
specG pitagoricas = do
  it "e1" $
    pitagoricas 10  `shouldBe`  [(3,4,5),(6,8,10)]
  it "e2" $
    pitagoricas 15  `shouldBe`  [(3,4,5),(5,12,13),(6,8,10),(9,12,15)]

spec :: Spec
spec = do
  describe "def. 1" $ specG pitagoricas1
  describe "def. 2" $ specG pitagoricas2
  describe "def. 3" $ specG pitagoricas3
