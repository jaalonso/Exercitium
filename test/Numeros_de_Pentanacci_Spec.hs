module Numeros_de_Pentanacci_Spec (main, spec) where

import Numeros_de_Pentanacci
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG pentanacci = 
  it "e1" $
    take 15 pentanacci `shouldBe`
    [0,1,1,2,4,8,16,31,61,120,236,464,912,1793,3525]

spec :: Spec
spec = do
  describe "def. 1" $ specG pentanacci1
  describe "def. 2" $ specG pentanacci2
  describe "def. 3" $ specG pentanacci3
  describe "def. 4" $ specG pentanacci4
