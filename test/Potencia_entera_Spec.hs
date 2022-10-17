module Potencia_entera_Spec (main, spec) where

import Potencia_entera
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer -> Integer) -> Spec
specG potencia =
  it "e1" $
    potencia 2 3  `shouldBe`  8

spec :: Spec
spec = do
  describe "def. 1" $ specG potencia1
  describe "def. 2" $ specG potencia2
  describe "def. 3" $ specG potencia3
  describe "def. 4" $ specG potencia4
  describe "def. 5" $ specG potencia5
  describe "def. 6" $ specG potencia6
  describe "def. 7" $ specG potencia7
  describe "def. 8" $ specG potencia8
  describe "def. 9" $ specG potencia9
  describe "equivalencia" $ it "p1" $ property prop_potencia
