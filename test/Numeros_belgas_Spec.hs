module Numeros_belgas_Spec (main, spec) where

import Numeros_belgas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> Int -> Bool) -> Spec
specG esBelga = do
  it "e1" $
    esBelga 0 18                     `shouldBe` True
  it "e2" $
    esBelga 1 19                     `shouldBe` False
  it "e3" $
    esBelga 0 2016                   `shouldBe` True
  it "e4" $
    [x | x <- [0..30], esBelga 7 x]  `shouldBe` [7,10,11,21,27,29]
  it "e5" $
    [x | x <- [0..30], esBelga 10 x] `shouldBe` [10,11,20,21,22,24,26]

spec :: Spec
spec = do
  describe "def. 1" $ specG esBelga1
  describe "def. 2" $ specG esBelga2
  describe "equivalencia" $ it "p1" $ property prop_esBelga
