module Ordenada_ciclicamente_Spec (main, spec) where

import Ordenada_ciclicamente
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> Maybe Int) -> Spec
specG ordenadaCiclicamente = do
  it "e1" $
    ordenadaCiclicamente [1,2,3,4]      `shouldBe`  Just 0
  it "e2" $
    ordenadaCiclicamente [5,8,1,3]      `shouldBe`  Just 2
  it "e3" $
    ordenadaCiclicamente [4,6,7,5,1,3]  `shouldBe`  Nothing
  it "e4" $
    ordenadaCiclicamente [1,0,3,2]      `shouldBe`  Nothing
  it "e5" $
    ordenadaCiclicamente [1,2,0]        `shouldBe`  Just 2

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadaCiclicamente1
  describe "def. 2" $ specG ordenadaCiclicamente2
  describe "def. 3" $ specG ordenadaCiclicamente3
  describe "equivalencia" $ it "p1" $ property prop_ordenadaCiclicamente
