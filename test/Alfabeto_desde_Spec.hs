module Alfabeto_desde_Spec (main, spec) where

import Alfabeto_desde
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Char -> String) -> Spec
specG alfabetoDesde = do
  it "e1" $
      alfabetoDesde 'e'  `shouldBe`  "efghijklmnopqrstuvwxyzabcd"
  it "e2" $
      alfabetoDesde 'a'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e3" $
      alfabetoDesde '7'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e4" $
      alfabetoDesde '{'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e5" $
      alfabetoDesde 'B'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "p1" $
    property prop_alfabetoDesde

spec :: Spec
spec = do
  describe "def. 1" $ specG alfabetoDesde1
  describe "def. 2" $ specG alfabetoDesde2
  describe "def. 3" $ specG alfabetoDesde3
  describe "def. 4" $ specG alfabetoDesde4
  describe "def. 5" $ specG alfabetoDesde5
