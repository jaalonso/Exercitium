module Indices_verdaderos_Spec (main, spec) where

import Indices_verdaderos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Bool]) -> Spec
specG indicesVerdaderos = do
  it "e1" $
    take 6 (indicesVerdaderos [1,4])
    `shouldBe` [False,True,False,False,True,False]
  it "e2" $
    take 6 (indicesVerdaderos [0,2..])
    `shouldBe` [True,False,True,False,True,False]
  it "e3" $
    take 3 (indicesVerdaderos [])
    `shouldBe` [False,False,False]
  it "e4" $
    take 6 (indicesVerdaderos [1..])
    `shouldBe` [False,True,True,True,True,True]

spec :: Spec
spec = do
  describe "def. 1" $ specG indicesVerdaderos1
  describe "def. 2" $ specG indicesVerdaderos2
  describe "def. 3" $ specG indicesVerdaderos3
  describe "def. 4" $ specG indicesVerdaderos4
  describe "def. 5" $ specG indicesVerdaderos5
  describe "def. 6" $ specG indicesVerdaderos6
  describe "equivalencia" $ it "p1" $ property prop_indicesVerdaderos
