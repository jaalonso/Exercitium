module Numeracion_con_multiples_base_Spec (main, spec) where

import Numeracion_con_multiples_base
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: ([Integer] -> Integer -> [Integer]) -> Spec
specG1 decimalAmultiple = do
  it "e1" $
    decimalAmultiple [2,4..] 377                      `shouldBe`  [7,5,0,1]
  it "e2" $
    decimalAmultiple [2,5..] 377                      `shouldBe`  [4,5,3,1]
  it "e3" $
    decimalAmultiple [2^n | n <- [1..]] 2015          `shouldBe`  [1,15,3,3,1]
  it "e4" $
    decimalAmultiple (repeat 10) 2015                 `shouldBe`  [2,0,1,5]

specG2 :: ([Integer] -> [Integer] -> Integer) -> Spec
specG2 multipleAdecimal = do
  it "e1" $
    multipleAdecimal [2,4..] [7,5,0,1]                `shouldBe`  377
  it "e2" $
    multipleAdecimal [2,5..] [4,5,3,1]                `shouldBe`  377
  it "e3" $
    multipleAdecimal [2^n | n <- [1..]] [1,15,3,3,1]  `shouldBe`  2015
  it "e4" $
    multipleAdecimal (repeat 10) [2,0,1,5]            `shouldBe`  2015

spec :: Spec
spec = do
  describe "def. 1" $ specG1 decimalAmultiple1
  describe "def. 2" $ specG1 decimalAmultiple2
  describe "def. 3" $ specG1 decimalAmultiple3
  describe "def. 4" $ specG2 multipleAdecimal1
  describe "def. 5" $ specG2 multipleAdecimal2
  describe "equivalencia" $ it "p1" $ property prop_decimalAmultiple
  describe "equivalencia" $ it "p2" $ property prop_multipleAdecimal
