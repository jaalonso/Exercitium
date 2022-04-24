module Representacion_de_Zeckendorf_Spec (main, spec) where

import Representacion_de_Zeckendorf
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG zeckendorf = do
  it "e1" $
    zeckendorf 100 `shouldBe` [89,8,3]
  it "e2" $
    zeckendorf 200 `shouldBe` [144,55,1]
  it "e3" $
    zeckendorf 300 `shouldBe` [233,55,8,3,1]

spec :: Spec
spec = do
  describe "def. 1" $ specG zeckendorf1
  describe "def. 2" $ specG zeckendorf2
  describe "def. 3" $ specG zeckendorf3
  describe "def. 4" $ specG zeckendorf4
  describe "equivalencia" $ it "p1" $ property prop_zeckendorf
