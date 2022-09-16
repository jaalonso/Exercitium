module Numeros_perfectos_Spec (main, spec) where

import Numeros_perfectos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG perfectos =
  it "e1" $
    perfectos 500 `shouldBe` [6,28,496]

spec :: Spec
spec = do
  describe "def. 1" $ specG perfectos1
  describe "def. 2" $ specG perfectos2
  describe "def. 3" $ specG perfectos3
  describe "def. 4" $ specG perfectos4
  describe "def. 5" $ specG perfectos5
  describe "def. 6" $ specG perfectos6
  describe "def. 7" $ specG perfectos7
  describe "equivalencia" $ it "p1" $ property prop_perfectos
