module Primos_circulares_Spec (main, spec) where

import Primos_circulares
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG circulares = do
  it "e1" $
    circulares !! 10 `shouldBe` 73 
  it "e1" $
    circulares !! 20 `shouldBe` 719

spec :: Spec
spec = do
  describe "def. 1" $ specG circulares1
  describe "def. 2" $ specG circulares2
  describe "def. 3" $ specG circulares3
  describe "def. 4" $ specG circulares4
