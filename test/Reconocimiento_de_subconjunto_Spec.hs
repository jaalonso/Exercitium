module Reconocimiento_de_subconjunto_Spec (main, spec) where

import Reconocimiento_de_subconjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int] -> Bool) -> Spec
specG subconjunto = do
  it "e1" $
    subconjunto [3,2,3] [2,5,3,5]  `shouldBe`  True
  it "e2" $
    subconjunto [3,2,3] [2,5,6,5]  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG subconjunto1
  describe "def. 2" $ specG subconjunto2
  describe "def. 3" $ specG subconjunto3
  describe "def. 4" $ specG subconjunto4
  describe "equivalencia" $ it "p1" $ property prop_subconjunto
