module Triangulares_con_cifras_Spec (main, spec) where

import Triangulares_con_cifras
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> [Integer]) -> Spec
specG triangularesConCifras = do
  it "e1" $
    take 6 (triangularesConCifras 1) `shouldBe` [1,3,6,55,66,666]
  it "e2" $
    take 6 (triangularesConCifras 2) `shouldBe` [10,15,21,28,36,45]

spec :: Spec
spec = do
  describe "def. 1" $ specG triangularesConCifras1
  describe "def. 2" $ specG triangularesConCifras2
  describe "def. 3" $ specG triangularesConCifras3
  describe "def. 4" $ specG triangularesConCifras4
  describe "def. 5" $ specG triangularesConCifras5
