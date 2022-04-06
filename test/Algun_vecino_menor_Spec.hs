module Algun_vecino_menor_Spec (main, spec) where

import Algun_vecino_menor
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Matriz -> [Int]) -> Spec
specG algunoMenor = do
  it "e1" $
    algunoMenor ej `shouldBe` [9,4,6,5,8,7,4,2,5,4]

spec :: Spec
spec = do
  describe "def. 1" $ specG algunoMenor1
  describe "def. 2" $ specG algunoMenor2
  describe "def. 3" $ specG algunoMenor3
  describe "def. 4" $ specG algunoMenor4
  describe "def. 5" $ specG algunoMenor5
  describe "equivalencia" $ it "p1" $ property prop_algunoMenor
