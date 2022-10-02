module Aproximacion_del_numero_e_Spec (main, spec) where

import Aproximacion_del_numero_e
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG1 :: (Int -> [Double]) -> Spec
specG1 aproxE =
  it "e1" $
    aproxE 4 `shouldBe` [2.0,2.25,2.37037037037037,2.44140625]

specG2 :: (Double -> Int) -> Spec
specG2 errorAproxE = do
  it "e1" $
    errorAproxE 0.1    `shouldBe`  13
  it "e1" $
    errorAproxE 0.01   `shouldBe`  135
  it "e1" $
    errorAproxE 0.001  `shouldBe`  1359


spec :: Spec
spec = do
  describe "def. 1" $ specG1 aproxE1
  describe "def. 1" $ specG1 aproxE2
  describe "def. 1" $ specG1 aproxE3
  describe "def. 1" $ specG1 aproxE4
  describe "def. 1" $ specG1 aproxE5
  describe "def. 1" $ specG2 errorAproxE1
  describe "def. 2" $ specG2 errorAproxE2
  describe "def. 3" $ specG2 errorAproxE3
  describe "equivalencia" $ it "p1" $ property prop_aproxE
  describe "equivalencia" $ it "p1" $ property prop_errorAproxE
